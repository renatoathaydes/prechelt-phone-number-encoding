use linked_hash_map::LinkedHashMap;
use plotters::prelude::*;
use plotters_svg::SVGBackend;
use std::env::args;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

struct DataPoint {
    run: u32,
    mem: u32,
    time: u32,
}

const OUT_FILE_NAME: &str = "chart.svg";
const COLORS: [&RGBColor; 5] = [
    &RED,
    &RGBColor(255, 165, 0),
    &RGBColor(0, 100, 0),
    &BLUE,
    &MAGENTA,
];

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = args().skip(1);
    let csv = args.next().expect("Missing argument: CSV File");

    let mut data: LinkedHashMap<String, Vec<DataPoint>> = LinkedHashMap::with_capacity(100);
    let mut max_mem: u32 = 100;
    let mut max_time: u32 = 100;
    let mut max_run: u32 = 0;

    for line in read_lines(csv)? {
        let entry = line?;
        let parts: Vec<_> = entry.split(',').collect();
        if parts[0] == "Proc" {
            continue;
        } // header
        let p = data_point(&parts)?;
        if p.mem > max_mem {
            max_mem = p.mem;
        }
        if p.time > max_time {
            max_time = p.time;
        }
        if p.run > max_run {
            max_run = p.run;
        }
        data.entry(parts[0].to_string())
            .or_insert_with(Vec::new)
            .push(p);
    }

    // add a little margin at the to of the charts
    max_mem += 20_000_000;
    max_time += 5_000;

    let root = SVGBackend::new(OUT_FILE_NAME, (512, 360)).into_drawing_area();
    root.fill(&WHITE)?;

    let mut chart = ChartBuilder::on(&root)
        .x_label_area_size(35)
        .y_label_area_size(40)
        .right_y_label_area_size(50)
        .margin(10)
        .caption(
            "Time and Memory Comparison",
            ("sans-serif", 32.0).into_font(),
        )
        .build_cartesian_2d(0..max_run, 0..max_time)?
        .set_secondary_coord(0..max_run, 0..max_mem);

    chart
        .configure_mesh()
        .y_desc("Time (secs)")
        .y_label_formatter(&|x| format!("{:.1}", x / 1000))
        .draw()?;

    chart
        .configure_secondary_axes()
        .y_desc("Memory (MB)")
        .y_label_formatter(&|x| format!("{:.0}", *x as f32 / 1e6f32))
        .draw()?;

    for (index, (proc, points)) in data.into_iter().enumerate() {
        let color = COLORS[index];

        chart
            .draw_series(LineSeries::new(
                points.iter().map(|p| (p.run, p.time)),
                color.stroke_width(3),
            ))?
            .label(format!("{} time", proc))
            .legend(move |(x, y)| {
                PathElement::new(vec![(x, y), (x + 20, y)], color.stroke_width(3))
            });

        chart
            .draw_secondary_series(LineSeries::new(
                points.iter().map(|p| (p.run, p.mem)),
                color,
            ))?
            .label(format!("{} memory", proc))
            .legend(move |(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], color));
    }

    chart
        .configure_series_labels()
        .border_style(&BLACK)
        .background_style(&RGBColor(220, 220, 220).mix(0.5))
        .position(SeriesLabelPosition::UpperMiddle)
        .draw()?;

    // To avoid the IO failure being ignored silently, we manually call the present function
    root.present().expect("Unable to write result to file, please make sure 'plotters-doc-data' dir exists under current dir");
    println!("Plot saved to {}", OUT_FILE_NAME);

    Ok(())
}

fn data_point(entry: &[&str]) -> Result<DataPoint, Box<dyn std::error::Error>> {
    let run = entry[1].parse::<u32>()?;
    let mem = entry[2].parse::<u32>()?;
    let time = entry[3].parse::<u32>()?;
    Ok(DataPoint { run, mem, time })
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
