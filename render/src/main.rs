use std::ops::{Add, Sub, Mul, Neg};
use std::option::{Option};
use rand;
use std::default::Default;

#[derive(Copy, Clone, Debug, Default)]
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
    let len = self.length();
    if len != 0.0 {
      self * (1. / len)
    }
    else {
      self
    }
  }

  fn neg(self) -> Vec3 {
    Vec3::new3(-self.x, -self.y, -self.z)
  }

  fn clear_z(self) -> Vec2 {
    Vec2::new2(self.x, self.y)
  }

  const ZERO: Vec3 = Vec3 { x: 0.0, y: 0.0, z: 0.0 };
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

impl Neg for Vec3 {
  type Output = Vec3;
  fn neg(self) -> Vec3 {
    Vec3::ZERO - self
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
  let canvas_width = (camera.horizontal_fov / 2.).tan() * camera.near_cap * 2.;
  let canvas_height = (camera.vertical_fov / 2.).tan() * camera.near_cap * 2.;
  Vec3::new3(p * x / canvas_width, p * y / canvas_height, (point - projected).length())
}

// fn angle_of(a: Vec3, zero: Vec3, up: Vec3) -> f32 { // up is really just up'ish
//   let theta = (a.normalize() * zero.normalize()).acos();
//   theta * (cross_product(zero, up).normalize() * a.normalize()).acos().signum()
// }
// 
// fn project_fisheye(point: Vec3, camera: Camera) -> Vec3 {
//   let center = camera.position;
//   let radius = camera.near_cap;
//   assert!((point - center).length() >= radius);
//   assert!((point - center).normalize() * camera.direction.normalize() > 0.0);
//   let equator_plane = Plane { point: center, normal: camera.up };
//   let point_on_equator_plane = intersect(Line { point, direction: camera.up }, equator_plane).unwrap();
//   let yaw = angle_of(point_on_equator_plane - center, camera.direction, camera.up);
//   let pitch = angle_of(point - center, point_on_equator_plane - center, cross_product(camera.up, camera.direction));
//   Vec3::new3(yaw / camera.horizontal_fov, pitch / camera.vertical_fov, (point - center).length() - radius)
// }

type Color = Vec3;

fn distance(point: Vec3, line: Line) -> Vec3 {
  let slope = point - line.point;
  let bot_len = slope * line.direction.normalize();
  let foot = line.point + line.direction.normalize() * bot_len;
  foot - point
}

fn is_point_inside_triangle(point: Vec3, a: Vec3, b: Vec3, c: Vec3) -> bool {
  let va = cross_product(b - a, point - a);
  let vb = cross_product(c - b, point - b);
  let vc = cross_product(a - c, point - c);
  va * vb > 0.0 && vb * vc > 0.0
}

fn average_triangle<T>(point: Vec3, a: Vec3, va: T, b: Vec3, vb: T, c: Vec3, vc: T) -> T
  where T: Mul<f32, Output = T> + Add<T, Output = T> {
  let da = point - a;
  let db = point - b;
  let dc = point - c;
  let pa = cross_product(db, dc).length();
  let pb = cross_product(dc, da).length();
  let pc = cross_product(da, db).length();
  let denom = pa + pb + pc;
  va * (pa / denom) + vb * (pb / denom) + vc * (pc / denom)
}

#[derive(Copy, Clone, Debug, Default)]
struct Vertex {
  position: Vec3,
  color: Color,
  normal: Vec3,
}

impl Vertex {
  fn map_position<F>(self, f: F) -> Vertex
    where F: FnOnce(Vec3) -> Vec3 {
    let mut v = self;
    v.position = f(v.position);
    v
  }

  fn with_normal(self, normal: Vec3) -> Vertex {
    let mut v = self;
    v.normal = normal;
    v
  }
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
  Vec3::ZERO
}

fn sample_triangles(point: Vec2, triangles: &[Vertex]) -> Color {
  let mut color = Vec3::ZERO;
  let mut depth = f32::MAX;
  for i in 0..triangles.len() - 2 {
    let a = triangles[i].position.clear_z();
    let b = triangles[i + 1].position.clear_z();
    let c = triangles[i + 2].position.clear_z();
    if is_point_inside_triangle(point, a, b, c) {
      let co = average_triangle(point, a, triangles[i].color, b, triangles[i + 1].color, c, triangles[i + 2].color);
      let de = average_triangle(point, a, triangles[i].position.z, b, triangles[i + 1].position.z, c, triangles[i + 2].position.z);
      if de < depth { color = co; depth = de; }
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
    projected_vertices.push(v.map_position(|p| project(p, camera)));
  }
  let dx = 1.0 / resolution_x as f32;
  let dy = 1.0 / resolution_y as f32;
  let bottom_left = Vec2::new2(-0.5, -0.5);
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

// fn random_jiggle(v: Vec3) -> Vec3 {
//   let r = v.length();
//   let mut w = Vec3::new3(0.0, 0.0, 0.0);
//   while v * w <= 0.0 {
//     let a = rand::random::<f32>() * 2.0 * 3.14;
//     let b = rand::random::<f32>() * 2.0 * 3.14;
//     w = Vec3::new3(r * a.sin() * b.cos(), r * a.sin() * b.sin(), r * a.cos());
//   }
//   w
// }

#[derive(Copy, Clone, Debug, Default)]
struct Triangle {
  a: Vertex,
  b: Vertex,
  c: Vertex,
  emitting: bool,
}

fn zip_product(a: Vec3, b: Vec3) -> Vec3 {
  Vec3::new3(a.x * b.x, a.y * b.y, a.z * b.z)
}

fn find_hit(point: Vec3, direction: Vec3, triangles: &[Triangle]) -> Option<(Vec3, Triangle)> {
  let mut depth = f32::MAX;
  let mut hitpoint = Vec3::ZERO;
  let mut target = Triangle::default();
  for t in triangles {
    let a = t.a.position;
    let b = t.b.position;
    let c = t.c.position;
    if let Some(p) = intersect(Line { point, direction }, Plane { point: a, normal: cross_product(a - b, a - c) }) {
      let n = average_triangle(p, t.a.position, t.a.normal, t.b.position, t.b.normal, t.c.position, t.c.normal).normalize();
      if (p - point) * direction > 0.0 && n * direction < 0.0 && is_point_inside_triangle(p, a, b, c) {
        let d = (p - point).length();
        if d < depth {
          hitpoint = p;
          depth = d;
          target = *t;
        }
      }
    }
  }
  if depth == f32::MAX { None } else { Some((hitpoint, target)) }
}

fn sample_ray(point: Vec3, direction: Vec3, triangles: &[Triangle]) -> Color {
  let mut dir = direction.normalize();
  let mut pos = point;
  let mut ret = Vec3::new3(1.0, 1.0, 1.0);
  let mut dist = 0.0;
  let mut num_iter = 0;
  loop {
    num_iter += 1;
    if num_iter > 100 {
      ret = Vec3::ZERO;
      break;
    }
    match find_hit(pos, dir, triangles) {
      None => {
        let height = dir.z;
        let skyblue = Vec3::new3(135.0, 206.0, 235.0) * (1.0 / 255.0);
        let a = (height + 1.0) / 2.0;
        ret = zip_product(ret, (1.0 - a) * Vec3::ZERO + a * skyblue);
        break;
      },
      Some((hitpoint, target)) => {
        dist += (pos - hitpoint).length();
        let normal = average_triangle(hitpoint, target.a.position, target.a.normal, target.b.position, target.b.normal, target.c.position, target.c.normal);
        let refl_dir = {
          let x = -2.0 * (dir * normal) / (normal * normal);
          dir + x * normal
        };
        let color = average_triangle(hitpoint, target.a.position, target.a.color, target.b.position, target.b.color, target.c.position, target.c.color);
        ret = zip_product(ret, color);
        if target.emitting { break; }
        dir = refl_dir;
        pos = hitpoint;
      }
    }
  }
  // if dist != 0.0 {
  //   ret = ret * (1.0 / (dist * dist));
  // }
  if ret.x >= 1.0 { ret.x = 1.0; }
  if ret.y >= 1.0 { ret.y = 1.0; }
  if ret.z >= 1.0 { ret.z = 1.0; }
  ret
}

fn ray_marching(camera: Camera, triangles: &[Triangle], resolution_x: i32, resolution_y: i32) -> Vec<Color> {
  let canvas_width = (camera.horizontal_fov / 2.).tan() * camera.near_cap * 2.;
  let canvas_height = (camera.vertical_fov / 2.).tan() * camera.near_cap * 2.;
  let origin = camera.position + camera.direction.normalize() * camera.near_cap;
  let y = camera.up.normalize();
  let x = cross_product(camera.direction, y).normalize();
  let dx = x * (canvas_width / resolution_x as f32);
  let dy = y * (canvas_height / resolution_y as f32);
  let bottom_left = origin - x * (canvas_width / 2.0) - y * (canvas_height / 2.0);
  let mut ret = Vec::new();
  for j in (0..resolution_y).rev() {
    for i in 0..resolution_x {
      let p = bottom_left + i as f32 * dx + j as f32 * dy;
      ret.push(sample_ray(p, p - camera.position, &triangles));
    }
  }
  ret
}

fn main() {
  let tlf = Vertex { position: Vec3::new3(-0.5, -0.5, 0.5),  color: Vec3::new3(0.0, 0.0, 1.0), normal: Vec3::ZERO };
  let tlb = Vertex { position: Vec3::new3(-0.5, 0.5, 0.5),   color: Vec3::new3(0.0, 1.0, 1.0), normal: Vec3::ZERO };
  let trf = Vertex { position: Vec3::new3(0.5, -0.5, 0.5),   color: Vec3::new3(1.0, 0.0, 1.0), normal: Vec3::ZERO };
  let trb = Vertex { position: Vec3::new3(0.5, 0.5, 0.5),    color: Vec3::new3(1.0, 1.0, 1.0), normal: Vec3::ZERO };
  let blf = Vertex { position: Vec3::new3(-0.5, -0.5, -0.5), color: Vec3::new3(0.0, 0.0, 0.0), normal: Vec3::ZERO };
  let blb = Vertex { position: Vec3::new3(-0.5, 0.5, -0.5),  color: Vec3::new3(0.0, 1.0, 0.0), normal: Vec3::ZERO };
  let brf = Vertex { position: Vec3::new3(0.5, -0.5, -0.5),  color: Vec3::new3(1.0, 0.0, 0.0), normal: Vec3::ZERO };
  let brb = Vertex { position: Vec3::new3(0.5, 0.5, -0.5),   color: Vec3::new3(1.0, 1.0, 0.0), normal: Vec3::ZERO };
  let cube_segments = vec![
    tlf, trf, brf, blf, tlf,
    tlb, trb, brb, blb, tlb,
    blb, blf,
    brf, brb, trb, trf
  ];
  let cube_triangles = vec![
    tlf, blf, trf, brf, brb, blf, blb, tlf, tlb, trf, trb, brb, tlb, blb
  ];
  let cube_complete_triangles = {
    let front = Vec3::new3(0.0, -1.0, 0.0);
    let back  = Vec3::new3(0.0, 1.0, 0.0);
    let left  = Vec3::new3(-1.0, 0.0, 0.0);
    let right = Vec3::new3(1.0, 0.0, 0.0);
    let up    = Vec3::new3(0.0, 0.0, 1.0);
    let down  = Vec3::new3(0.0, 0.0, -1.0);
    let brightness = 1.0;
    let sky_len = 1e7;
    vec![
      Triangle { emitting: false, a: tlf.with_normal(front), b: trf.with_normal(front), c: blf.with_normal(front) },
      Triangle { emitting: false, a: blf.with_normal(front), b: brf.with_normal(front), c: trf.with_normal(front) },
      Triangle { emitting: false, a: tlb.with_normal(back), b: trb.with_normal(back), c: blb.with_normal(back) },
      Triangle { emitting: false, a: blb.with_normal(back), b: brb.with_normal(back), c: trb.with_normal(back) },
      Triangle { emitting: false, a: tlb.with_normal(left), b: tlf.with_normal(left), c: blf.with_normal(left) },
      Triangle { emitting: false, a: tlb.with_normal(left), b: blb.with_normal(left), c: blf.with_normal(left) },
      Triangle { emitting: false, a: trb.with_normal(right), b: trf.with_normal(right), c: brf.with_normal(right) },
      Triangle { emitting: false, a: trb.with_normal(right), b: brb.with_normal(right), c: brf.with_normal(right) },
      Triangle { emitting: false, a: tlf.with_normal(up), b: trf.with_normal(up), c: tlb.with_normal(up) },
      Triangle { emitting: false, a: trb.with_normal(up), b: trf.with_normal(up), c: tlb.with_normal(up) },
      Triangle { emitting: false, a: blf.with_normal(down), b: brf.with_normal(down), c: blb.with_normal(down) },
      Triangle { emitting: false, a: brb.with_normal(down), b: brf.with_normal(down), c: blb.with_normal(down) },

      // Triangle {
      //   emitting: true,
      //   a: Vertex { position: Vec3::new3(-sky_len, -sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      //   b: Vertex { position: Vec3::new3(sky_len, -sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      //   c: Vertex { position: Vec3::new3(0.0, sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      // },
      // Triangle {
      //   emitting: false,
      //   a: Vertex { position: Vec3::new3(-sky_len, -sky_len, -1.0), color: Vec3::new3(1.0, 1.0, 1.0), normal: down },
      //   b: Vertex { position: Vec3::new3(sky_len, -sky_len, -1.0), color: Vec3::new3(1.0, 1.0, 1.0), normal: down },
      //   c: Vertex { position: Vec3::new3(0.0, sky_len, -1.0), color: Vec3::new3(1.0, 1.0, 1.0), normal: down },
      // },
    ]
  };
  let horizontal_fov = 3.14 * 0.4;
  let camera = Camera {
    position: Vec3::new3(1.0, -2.0, 1.0),
    direction: Vec3::new3(-1.5, 3.0, -1.5),
    up: Vec3::new3(-1.0 / 3.0, 1.0 / 3.0, 1.0),
    // position: Vec3::new3(-3., 0., 0.),
    // direction: Vec3::new3(1., 0., 0.),
    // up: Vec3::new3(0., 0., 1.),
    // position: Vec3::new3(0.0, 0.0, 2.0),
    // direction: Vec3::new3(0.0, 0.0, -1.0),
    // up: Vec3::new3(0.0, 1.0, 0.0),
    near_cap: 0.5,
    far_cap: 1000.0,
    horizontal_fov,
    vertical_fov: ((horizontal_fov / 2.0).tan() * (9.0 / 16.0)).atan() * 2.0,
  };
  let width = 1920;
  let height = 1080;
  // let pic = rasterize(camera, &cube_triangles, width, height, RenderType::Triangles);
  let pic = ray_marching(camera, &cube_complete_triangles, width, height);
  println!("P3 {} {} 255\n", width, height);
  for i in pic {
    println!("{} {} {}", (i.x * 255.0) as i32, (i.y * 255.0) as i32, (i.z * 255.0) as i32);
  }
}
