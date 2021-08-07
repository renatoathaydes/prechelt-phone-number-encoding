use std::cmp::max;
use std::env::args;
use std::process::*;
use std::thread::sleep;
use std::time::{Duration, Instant};

use libproc::libproc::pid_rusage::{pidrusage, RUsageInfoV4};

fn main() {
    let mut args = args().skip(1);
    let input_desc = args.next().expect("Description of input was not provided");
    let proc_name = args.next().expect("Name of process to start was not provided");
    let proc_args: Vec<_> = args.collect();

    let start_time = Instant::now();

    let mut out = Command::new(&proc_name).args(proc_args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .spawn()
        .expect("process could not be created");
    let pid = out.id() as i32;

    let mut mem: u64 = 0;

    loop {
        if let Some(status) = out.try_wait().expect("unable to wait for process") {
            if !status.success() {
                eprintln!("Process exited with status {:?}", status.code()
                    .expect("cannot obtain exit code"));
                exit(1);
            }
            break;
        } else {
            // the program seems to be still running, but we can still fail below
            match pidrusage::<RUsageInfoV4>(pid) {
                Ok(info) => {
                    mem = max(mem, info.ri_resident_size);
                }
                Err(_) => break // ignore error because the process must have ended already
            }
            sleep(Duration::from_millis(5));
        }
    }

    let total_time = Instant::now().duration_since(start_time);

    println!("Proc,Input,Memory(bytes),Time(ms)");
    println!("{},{},{},{}", proc_name, input_desc, mem, total_time.as_millis());
}
