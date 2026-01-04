use crate::haversine_distance::haversine;
use rand::{Rng, SeedableRng};
use rand_pcg::{Lcg64Xsh32, Pcg32};
use std::fs;

pub fn generate(mode: &String, seed: u64, pairs: u64) -> f64 {
    let mut rng = Pcg32::seed_from_u64(seed);

    let (avg, json) = if mode == "uniform" {
        generate_uniform(pairs, &mut rng)
    } else if mode == "cluster" {
        generate_clustered(pairs, &mut rng)
    } else {
        println!("Unknown mode {}", mode);
        (-1.0, "".to_string())
    };

    fs::write("../input.json", json).expect("Should be able to write to input.json");
    fs::write("../expected.f64", avg.to_string()).expect("Should be able to write to expected.f64");

    avg
}

fn generate_uniform(pairs: u64, rand: &mut Lcg64Xsh32) -> (f64, String) {
    let earth_radius: f64 = 6372.8;
    let mut avg: f64 = 0.0;
    let mut json: Vec<String> = vec![];
    let avg_coef = 1.0 / (pairs as f64);

    json.push("{\"pairs\": [".to_string());

    for _ in 0..pairs {
        let x0 = -180.0 + rand.random::<f64>() * 360.0;
        let y0 = -90.0 + rand.random::<f64>() * 180.0;
        let x1 = -180.0 + rand.random::<f64>() * 360.0;
        let y1 = -90.0 + rand.random::<f64>() * 180.0;

        avg += haversine(x0, y0, x1, y1, earth_radius) * avg_coef;
        json.push(format!(
            "{{\"x0\":{}, \"y0\":{}, \"x1\":{}, \"y1\":{}}},\n",
            x0, y0, x1, y1
        ));
    }

    json.push("]}".to_string());

    (avg, json.join(""))
}

fn generate_clustered(pairs: u64, rand: &mut Lcg64Xsh32) -> (f64, String) {
    let earth_radius: f64 = 6372.8;
    let mut avg: f64 = 0.0;
    let mut json: Vec<String> = vec![];
    let avg_coef = 1.0 / (pairs as f64);
    let cluster_amount = 64;
    let cluster_radius_x = 5.0;
    let cluster_radius_y = 10.0;
    let clusters = (0..cluster_amount)
        .into_iter()
        .map(|_| {
            let x = -180.0 + rand.random::<f64>() * 360.0;
            let y = -90.0 + rand.random::<f64>() * 180.0;
            (x, y)
        })
        .collect::<Vec<(f64, f64)>>();

    json.push("{\"pairs\": [".to_string());

    for i in 0..pairs {
        let (center_x, center_y) = clusters[(i % cluster_amount) as usize];

        let x0 = center_x - cluster_radius_x + rand.random::<f64>() * (2.0 * cluster_radius_x);
        let y0 = center_y - cluster_radius_y + rand.random::<f64>() * (2.0 * cluster_radius_y);
        let x1 = center_x - cluster_radius_x + rand.random::<f64>() * (2.0 * cluster_radius_x);
        let y1 = center_y - cluster_radius_y + rand.random::<f64>() * (2.0 * cluster_radius_y);

        avg += haversine(x0, y0, x1, y1, earth_radius) * avg_coef;
        json.push(format!(
            "{{\"x0\":{}, \"y0\":{}, \"x1\":{}, \"y1\":{}}},\n",
            x0, y0, x1, y1
        ));
    }

    json.push("]}".to_string());

    (avg, json.join(""))
}
