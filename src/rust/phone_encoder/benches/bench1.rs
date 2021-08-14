use core::iter;

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main, Throughput};

use phone_encoder::print_solution;

static NUM_SRC: [&str; 7] = ["123456", "78901234", "567890", "123456789", "012345", "43210", "98765432"];

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

fn bench_print_solution(c: &mut Criterion) {
    let mut group = c.benchmark_group("phone_encoder");
    for words in [1, 10, 20, 30, 40, 50].iter()
        .map(|size| generate_data(*size as usize, &WORD_SRC)) {
        let num = generate_data(1, &NUM_SRC).remove(0);
        let size = words.len() as u64;
        group.throughput(Throughput::Elements(size));
        group.bench_with_input(BenchmarkId::from_parameter(size), &(num, words), |b, (num, words)| {
            b.iter(|| {
                print_solution(num, words);
            });
        });
    }
    group.finish();
}

criterion_group!(benches, bench_print_solution);
criterion_main!(benches);