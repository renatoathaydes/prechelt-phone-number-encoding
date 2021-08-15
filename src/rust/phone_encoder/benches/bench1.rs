use core::iter;

use criterion::{BenchmarkId, black_box, Criterion, criterion_group, criterion_main, Throughput};

static WORD_SRC: [&str; 26] = ["avocado", "band", "cereal", "dialog", "egg", "french",
    "gorilla", "human", "india", "julia", "kilogram", "laureate", "magic", "nothing", "ostensible",
    "put", "queue", "richness", "state", "television", "universal", "variety", "xman", "zumba",
    "watch", "you"];

fn generate_data(size: usize, source: &[&'static str]) -> Vec<&'static str> {
    let mut vec = Vec::with_capacity(size);
    for i in 0..size {
        vec.push(source[i % source.len()]);
    }
    vec
}

fn bench_iterate_over_string_characters(c: &mut Criterion) {
    let mut group = c.benchmark_group("str_iter");
    for words in [1, 10, 20, 30, 40, 50].iter()
        .map(|size| generate_data(*size as usize, &WORD_SRC)) {
        let size = words.len() as u64;
        group.throughput(Throughput::Elements(size));
        group.bench_with_input(BenchmarkId::from_parameter(size), &words, |b, words| {
            b.iter(|| {
                for ch in words.iter().flat_map(|w| w.chars()) {
                    let _ = black_box(ch);
                }
            });
        });
    }
    group.finish();
}

criterion_group!(benches, bench_iterate_over_string_characters);
criterion_main!(benches);