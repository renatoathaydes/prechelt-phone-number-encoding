use core::iter;
use std::io::{self, BufWriter};

use criterion::{BenchmarkId, black_box, Criterion, criterion_group, criterion_main, Throughput};
use smallvec::SmallVec;

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
        let stderr = io::stderr();
        let mut out = BufWriter::new(stderr.lock());
        group.bench_with_input(BenchmarkId::from_parameter(size), &(num, words), |b, (num, words)| {
            b.iter(|| {
                print_solution(num, words, &mut out);
            });
        });
    }
    group.finish();
}

fn bench_vec_mut(c: &mut Criterion) {
    let mut group = c.benchmark_group("vec_push_pop");
    for words in [1, 10, 20, 30, 40, 50].iter()
        .map(|size| generate_data(*size as usize, &WORD_SRC)) {
        let size = words.len() as u64;
        group.throughput(Throughput::Elements(size));
        group.bench_with_input(BenchmarkId::from_parameter(size), &words, |b, words| {
            b.iter(|| {
                let mut vec = Vec::with_capacity(50);
                for word in words {
                    vec.push(black_box(word));
                }
                for _ in 0..words.len() {
                    black_box(&vec);
                    vec.pop();
                }
                vec
            });
        });
    }
    group.finish();
}

criterion_group!(benches, bench_vec_mut);
criterion_main!(benches);