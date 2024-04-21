use std::ops::{Add, Sub, Mul};
use std::option::{Option};

#[derive(Copy, Clone, Debug)]
struct Vec3 {
  x: f32,
  y: f32,
  z: f32,
}

type Vec2 = Vec3;

impl Vec3 {
  fn new3(x: f32, y: f32, z: f32) -> Vec3 {
    Vec3 { x, y, z }
  }

  fn new2(x: f32, y: f32) -> Vec3 {
    Vec3 { x, y, z: 0. }
  }

  fn length(self) -> f32 {
    (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
  }

  fn normalize(self) -> Vec3 {
    self * (1. / self.length())
  }

  fn neg(self) -> Vec3 {
    Vec3::new3(-self.x, -self.y, -self.z)
  }
}

impl Add for Vec3 {
  type Output = Vec3;
  fn add(self, o: Vec3) -> Vec3 {
    Vec3 { x: self.x + o.x, y: self.y + o.y, z: self.z + o.z }
  }
}

impl Sub for Vec3 {
  type Output = Vec3;
  fn sub(self, o: Vec3) -> Vec3 {
    self + o.neg()
  }
}

impl Mul for Vec3 {
  type Output = f32;
  fn mul(self, o: Vec3) -> f32 {
    self.x * o.x + self.y * o.y + self.z * o.z
  }
}

impl Mul<f32> for Vec3 {
  type Output = Vec3;
  fn mul(self, o: f32) -> Vec3 {
    Vec3 { x: self.x * o, y: self.y * o, z: self.z * o }
  }
}

impl Mul<Vec3> for f32 {
  type Output = Vec3;
  fn mul(self, o: Vec3) -> Vec3 {
    o * self
  }
}

#[derive(Copy, Clone, Debug)]
struct Camera {
  position: Vec3, // where do you stand?
  direction: Vec3, // where are you looking at?
  up: Vec3, // where do your head point at?
  near_cap: f32,
  far_cap: f32, // not used really
  horizontal_fov: f32,
  vertical_fov: f32
}

#[derive(Copy, Clone, Debug)]
struct Plane {
  point: Vec3,
  normal: Vec3
}

#[derive(Copy, Clone, Debug)]
struct Line {
  point: Vec3,
  direction: Vec3
}

fn intersect(line: Line, plane: Plane) -> Option<Vec3> {
  let n = plane.normal;
  let d = line.direction;
  let p = line.point;
  let q = plane.point;
  if (d * n).abs() < 0.00001 {
    None
  }
  else {
    let a = ((q + p.neg()) * n) / (d * n);
    Some(p + a * d)
  }
}

fn signed_distance(point: Vec3, plane: Plane) -> f32 {
  (point - plane.point) * plane.normal.normalize()
}

fn cross_product(a: Vec3, b: Vec3) -> Vec3 {
  Vec3 {
    x: a.y * b.z - a.z * b.y,
    y: a.z * b.x - a.x * b.z,
    z: a.x * b.y - a.y * b.x
  }
}

fn project(point: Vec3, camera: Camera) -> Vec2 {
  let origin = camera.position + camera.direction.normalize() * camera.near_cap;
  let plane = Plane { point: origin, normal: camera.direction };
  assert!(signed_distance(point, plane) * signed_distance(camera.position, plane) <= 0.);
  let projected = intersect(Line { point, direction: point - camera.position }, plane).unwrap();
  let y = camera.up.normalize();
  let x = cross_product(camera.direction, y).normalize();
  let p = projected - origin;
  Vec2::new2(p * x, p * y)
}

type Color = bool;

const BLACK: bool = true;
const WHITE: bool = false;

fn distance(point: Vec3, line: Line) -> f32 {
  let slope = point - line.point;
  let bot_len = slope * line.direction.normalize();
  (slope * slope - bot_len * bot_len).sqrt()
}

fn sample(point: Vec2, segments: &Vec<Vec2>, epsilon: f32) -> Color {
  let mut hit = false;
  for i in 0..segments.len() - 1 {
    let a = segments[i];
    let b = segments[i + 1];
    if (point - a) * (b - a) < 0. || (point - b) * (a - b) < 0. { continue; }
    if distance(point, Line { point: a, direction: b - a }) < epsilon {
      hit = true;
      break;
    }
  }
  if hit { BLACK } else { WHITE }
}

fn rasterize(camera: Camera, segments: &Vec<Vec2>, resolution_x: i32, resolution_y: i32) -> Vec<Color> {
  let mut projected_segments = Vec::new();
  for v in segments {
    projected_segments.push(project(*v, camera));
  }
  let canvas_width = (camera.horizontal_fov / 2.).sin() * camera.near_cap * 2.;
  let canvas_height = (camera.vertical_fov / 2.).sin() * camera.near_cap * 2.;
  let dx = canvas_width / resolution_x as f32;
  let dy = canvas_height / resolution_y as f32;
  let bottom_left = Vec2::new2(-canvas_width / 2., -canvas_height / 2.);
  let mut ret = Vec::new();
  let epsilon = (dx * dx + dy * dy).sqrt();
  for j in (0..resolution_y).rev() {
    for i in 0..resolution_x {
      let p = bottom_left + Vec2::new2(i as f32 * dx, j as f32 * dy);
      ret.push(sample(p, &projected_segments, epsilon));
    }
  }
  ret
}

fn main() {
  let tlf = Vec3::new3(-0.5, -0.5, 0.5);
  let tlb = Vec3::new3(-0.5, 0.5, 0.5);
  let trf = Vec3::new3(0.5, -0.5, 0.5);
  let trb = Vec3::new3(0.5, 0.5, 0.5);
  let blf = Vec3::new3(-0.5, -0.5, -0.5);
  let blb = Vec3::new3(-0.5, 0.5, -0.5);
  let brf = Vec3::new3(0.5, -0.5, -0.5);
  let brb = Vec3::new3(0.5, 0.5, -0.5);
  let cube = vec![
    tlf, trf, brf, blf, tlf,
    tlb, trb, brb, blb, tlb,
    blb, blf,
    brf, brb, trb, trf
  ];
  let camera = Camera {
    position: Vec3::new3(3.0, -6.0, 3.0),
    direction: Vec3::new3(-1.5, 3.0, -1.5),
    up: Vec3::new3(-1.0 / 3.0, 1.0 / 3.0, 1.0),
    // position: Vec3::new3(0., -3., 0.),
    // direction: Vec3::new3(0., 1., 0.),
    // up: Vec3::new3(0., 0., 1.),
    near_cap: 0.25,
    far_cap: 1000.,
    horizontal_fov: 3.14 / 6.,
    vertical_fov: 3.14 / 6. / 16. * 9.,
  };
  let pic = rasterize(camera, &cube, 1920, 1080);
  println!("P3 1920 1080 1\n");
  for i in pic {
    if i == BLACK { println!(" 1 1 1"); }
    else { println!(" 0 0 0"); }
  }
}
