pub fn haversine(x0: f64, y0: f64, x1: f64, y1: f64, earth_radius: f64) -> f64 {
    let lat1 = y0;
    let lat2 = y1;
    let lon1 = x0;
    let lon2 = x1;

    let delta_lat = to_radians(lat2 - lat1);
    let delta_lon = to_radians(lon2 - lon1);
    let a = (delta_lat / 2.0).sin().powi(2) + to_radians(lat1).cos() * to_radians(lat2).cos() * (delta_lon / 2.0).sin().powi(2);
    let c = 2.0 * a.sqrt().atan2((1.0 - a).sqrt());

    earth_radius * c
}

fn to_radians(degrees: f64) -> f64 {
    degrees * 0.01745329251994329577
}