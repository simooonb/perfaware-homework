use crate::haversine_distance::haversine;
use crate::platform_metrics::{read_cpu_freq, read_cpu_timer};
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};

struct PointPair {
    x0: f64,
    x1: f64,
    y0: f64,
    y1: f64,
}

pub fn process(file_name: &String) -> f64 {
    let start_timer = read_cpu_timer();

    let file = File::open(file_name).unwrap();
    let reader = BufReader::new(file);
    let file_read_timer = read_cpu_timer();

    let mut ignored = HashSet::new();
    ignored.insert(' ');
    ignored.insert('\n');

    let global_start = '[';
    let global_end = ']';
    let object_start = '{';
    let object_end = '}';
    let delimiter = ',';

    let mut found_start = false;
    let mut found_end = false;
    let mut parsing_object = false;
    let mut buffer = vec![];
    let mut pairs = vec![];

    let vars_setup_timer = read_cpu_timer();

    for maybe_line in reader.lines() {
        let line = maybe_line.unwrap();

        for current in line.chars() {
            if !ignored.contains(&current) {
                if !found_start && current == global_start {
                    found_start = true;
                } else if !found_end && current == global_end {
                    found_end = true;
                } else if found_start && !found_end {
                    if !parsing_object && current == delimiter {
                        ()
                    } else if !parsing_object && current == object_start {
                        parsing_object = true;
                    } else if parsing_object && current == object_end {
                        pairs.push(parse_buffer(buffer));
                        parsing_object = false;
                        buffer = vec![];
                    } else {
                        buffer.push(current);
                    }
                }
            }
        }
    }

    let parsing_timer = read_cpu_timer();

    let mut sum = 0.0;
    let size = pairs.len();
    let avg_coef = 1.0 / (size as f64);
    let earth_radius: f64 = 6372.8;

    for pair in pairs {
        sum += haversine(pair.x0, pair.y0, pair.x1, pair.y1, earth_radius) * avg_coef;
    }

    let sum_timer = read_cpu_timer();

    let total_time = sum_timer - start_timer;
    let read_file_time = file_read_timer - start_timer;
    let vars_setup_time = vars_setup_timer - file_read_timer;
    let parsing_time = parsing_timer - vars_setup_timer;
    let sum_time = sum_timer - parsing_timer;
    let cpu_freq = read_cpu_freq();

    println!(
        "File read: {} ({:.2?}%)\nVars setup: {} ({:.2?}%)\nParsing: {} ({:.2?}%)\nSum: {} ({:.2?}%)\nTotal: {}\nTotal time: {:.3}s (CPU Freq {})",
        read_file_time,
        100.0 * read_file_time as f64 / total_time as f64,
        vars_setup_time,
        100.0 * vars_setup_time as f64 / total_time as f64,
        parsing_time,
        100.0 * parsing_time as f64 / total_time as f64,
        sum_time,
        100.0 * sum_time as f64 / total_time as f64,
        total_time,
        total_time as f64 / cpu_freq as f64,
        cpu_freq
    );

    sum
}

fn parse_buffer(buffer: Vec<char>) -> PointPair {
    let str = buffer.iter().collect::<String>();
    let split = str.split(",");

    let mut x0 = 0.0;
    let mut x1 = 0.0;
    let mut y0 = 0.0;
    let mut y1 = 0.0;

    for field in split {
        let field_split = field
            .split("\"")
            .filter(|sub| !sub.is_empty())
            .collect::<Vec<&str>>();

        let value = field_split[1][1..].parse::<f64>().unwrap();

        match field_split[0] {
            "x0" => x0 = value,
            "y0" => y0 = value,
            "x1" => x1 = value,
            "y1" => y1 = value,
            _ => (),
        };
    }

    PointPair { x0, y0, x1, y1 }
}
