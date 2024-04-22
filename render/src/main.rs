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

  fn clear_z(self) -> Vec2 {
    Vec2::new2(self.x, self.y)
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

fn project(point: Vec3, camera: Camera) -> Vec3 { // (x, y, depth)
  let origin = camera.position + camera.direction.normalize() * camera.near_cap;
  let plane = Plane { point: origin, normal: camera.direction };
  assert!(signed_distance(point, plane) * signed_distance(camera.position, plane) <= 0.);
  let projected = intersect(Line { point, direction: point - camera.position }, plane).unwrap();
  let y = camera.up.normalize();
  let x = cross_product(camera.direction, y).normalize();
  let p = projected - origin;
  Vec3::new3(p * x, p * y, (point - projected).length())
}

type Color = Vec3;

fn distance(point: Vec3, line: Line) -> Vec3 {
  let slope = point - line.point;
  let bot_len = slope * line.direction.normalize();
  let foot = line.point + line.direction.normalize() * bot_len;
  foot - point
}

#[derive(Copy, Clone, Debug)]
struct Vertex {
  position: Vec3,
  color: Color,
}

fn sample_segments(point: Vec2, segments: &[Vertex], epsilon: f32) -> Color {
  for i in 0..segments.len() - 1 {
    let a = segments[i].position.clear_z();
    let b = segments[i + 1].position.clear_z();
    if (point - a) * (b - a) < 0. || (point - b) * (a - b) < 0. { continue; }
    let dist = distance(point, Line { point: a, direction: b - a });
    if dist.length() < epsilon {
      let foot = point + dist;
      let da = (foot - a).length();
      let db = (foot - b).length();
      return segments[i].color * (db / (da + db)) + segments[i + 1].color * (da / (da + db));
    }
  }
  Vec3::new3(0.0, 0.0, 0.0)
}

fn sample_triangles(point: Vec2, triangles: &[Vertex]) -> Color {
  let mut color = Vec3::new3(0.0, 0.0, 0.0);
  let mut depth = f32::MAX;
  for i in 0..triangles.len() - 2 {
    let a = triangles[i].position.clear_z();
    let b = triangles[i + 1].position.clear_z();
    let c = triangles[i + 2].position.clear_z();
    let va = cross_product(b - a, point - a);
    let vb = cross_product(c - b, point - b);
    let vc = cross_product(a - c, point - c);
    if va * vb > 0.0 && vb * vc > 0.0 {
      let da = point - a;
      let db = point - b;
      let dc = point - c;
      let pa = cross_product(db, dc).length();
      let pb = cross_product(dc, da).length();
      let pc = cross_product(da, db).length();
      let denom = pa + pb + pc;
      let c = triangles[i].color * (pa / denom)
        + triangles[i + 1].color * (pb / denom)
        + triangles[i + 2].color * (pc / denom);
      let d = triangles[i].position.z * (pa / denom)
        + triangles[i + 1].position.z * (pb / denom)
        + triangles[i + 2].position.z * (pc / denom);
      if d < depth { color = c; depth = d; }
    }
  }
  color
}

enum RenderType {
  Triangles,
  Segments,
}

fn rasterize(camera: Camera, vertices: &[Vertex], resolution_x: i32, resolution_y: i32, render_type: RenderType) -> Vec<Color> {
  let mut projected_vertices = Vec::new();
  for v in vertices {
    projected_vertices.push(Vertex { position: project(v.position, camera), color: v.color });
  }
  let canvas_width = (camera.horizontal_fov / 2.).tan() * camera.near_cap * 2.;
  let canvas_height = (camera.vertical_fov / 2.).tan() * camera.near_cap * 2.;
  let dx = canvas_width / resolution_x as f32;
  let dy = canvas_height / resolution_y as f32;
  let bottom_left = Vec2::new2(-canvas_width / 2., -canvas_height / 2.);
  let mut ret = Vec::new();
  let epsilon = (dx * dx + dy * dy).sqrt();
  for j in (0..resolution_y).rev() {
    for i in 0..resolution_x {
      let p = bottom_left + Vec2::new2(i as f32 * dx, j as f32 * dy);
      match render_type {
        RenderType::Segments => ret.push(sample_segments(p, &projected_vertices, epsilon)),
        RenderType::Triangles => ret.push(sample_triangles(p, &projected_vertices)),
      }
    }
  }
  ret
}

fn main() {
  let tlf = Vertex { position: Vec3::new3(-0.5, -0.5, 0.5), color: Vec3::new3(0.0, 0.0, 1.0) };
  let tlb = Vertex { position: Vec3::new3(-0.5, 0.5, 0.5), color: Vec3::new3(0.0, 1.0, 1.0) };
  let trf = Vertex { position: Vec3::new3(0.5, -0.5, 0.5), color: Vec3::new3(1.0, 0.0, 1.0) };
  let trb = Vertex { position: Vec3::new3(0.5, 0.5, 0.5), color: Vec3::new3(1.0, 1.0, 1.0) };
  let blf = Vertex { position: Vec3::new3(-0.5, -0.5, -0.5), color: Vec3::new3(0.0, 0.0, 0.0) };
  let blb = Vertex { position: Vec3::new3(-0.5, 0.5, -0.5), color: Vec3::new3(0.0, 1.0, 0.0) };
  let brf = Vertex { position: Vec3::new3(0.5, -0.5, -0.5), color: Vec3::new3(1.0, 0.0, 0.0) };
  let brb = Vertex { position: Vec3::new3(0.5, 0.5, -0.5), color: Vec3::new3(1.0, 1.0, 0.0) };
  let cube_segments = vec![
    tlf, trf, brf, blf, tlf,
    tlb, trb, brb, blb, tlb,
    blb, blf,
    brf, brb, trb, trf
  ];
  let cube_triangles = vec![
    tlf, blf, trf, brf, brb, blf, blb, tlf, tlb, trf, trb, brb, tlb, blb
  ];
  let horizontal_fov = 3.14 * 0.6;
  let camera = Camera {
    position: Vec3::new3(1.0, -2.0, 1.0),
    direction: Vec3::new3(-1.5, 3.0, -1.5),
    up: Vec3::new3(-1.0 / 3.0, 1.0 / 3.0, 1.0),
    // position: Vec3::new3(-3., 0., 0.),
    // direction: Vec3::new3(1., 0., 0.),
    // up: Vec3::new3(0., 0., 1.),
    near_cap: 0.5,
    far_cap: 1000.0,
    horizontal_fov,
    vertical_fov: ((horizontal_fov / 2.0).tan() * (9.0 / 16.0)).atan() * 2.0,
  };
  let pic = rasterize(camera, &cube_triangles, 1920, 1080, RenderType::Triangles);
  println!("P3 1920 1080 255\n");
  for i in pic {
    println!("{} {} {}", (i.x * 255.0) as i32, (i.y * 255.0) as i32, (i.z * 255.0) as i32);
  }
}
