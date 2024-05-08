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
  const ONE: Vec3 = Vec3 { x: 1.0, y: 1.0, z: 1.0 };
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
    projected_vertices.push(Vertex { position: project(v.position, camera), .. *v});
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

fn random_unit_vector() -> Vec3 {
  loop {
    let x = rand::random::<f32>() * 2.0 - 1.0;
    let y = rand::random::<f32>() * 2.0 - 1.0;
    let z = rand::random::<f32>() * 2.0 - 1.0;
    let v = Vec3::new3(x, y, z);
    if v.length() <= 1.0 {
      return v.normalize();
    }
  }
}

#[derive(Copy, Clone, Debug, Default)]
struct Triangle {
  a: Vertex,
  b: Vertex,
  c: Vertex,
  emitting: bool,
  matt: i32,
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
      if (p - point) * direction > 0.0 && (p - point).length() > 0.00001 && is_point_inside_triangle(p, a, b, c) {
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

fn sample_ray(point: Vec3, direction: Vec3, triangles: &[Triangle], max_hit: i32) -> Color {
  if max_hit <= 0 { return Vec3::ONE; }
  let mut dir = direction.normalize();
  match find_hit(point, dir, triangles) {
    None => {
      let height = dir.z;
      let skyblue = Vec3::new3(135.0, 206.0, 235.0) * (1.0 / 255.0);
      let a = (height + 1.0) / 2.0;
      (1.0 - a) * Vec3::ZERO + a * skyblue
    },
    Some((hitpoint, target)) => {
      let normal = average_triangle(hitpoint, target.a.position, target.a.normal, target.b.position, target.b.normal, target.c.position, target.c.normal);
      let color = average_triangle(hitpoint, target.a.position, target.a.color, target.b.position, target.b.color, target.c.position, target.c.color);
      if target.emitting {
        color
      }
      else if target.matt > 0 {
        let mut next_color = Vec3::ZERO;
        for _ in 0 .. target.matt {
          let refl_dir = normal.normalize() + random_unit_vector() - hitpoint;
          next_color = next_color + sample_ray(hitpoint, refl_dir, triangles, max_hit - 1);
        }
        next_color = next_color * (1.0 / target.matt as f32);
        zip_product(color, next_color)
      }
      else {
        let refl_dir = {
          let x = -2.0 * (dir * normal) / (normal * normal);
          dir + x * normal
        };
        zip_product(color, sample_ray(hitpoint, refl_dir, triangles, max_hit - 1))
      }
    }
  }
}

fn gamma_correct(color: Color) -> Color {
  Vec3::new3(color.x.sqrt(), color.y.sqrt(), color.z.sqrt())
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
      let mut color = Vec3::ZERO;
      for s in 0..10 {
        let theta = s as f32 * 2.0 * 3.14 / 10.0;
        let dp = dx * 0.3 * theta.cos() + dy * 0.3 * theta.sin();
        color = color + sample_ray(p + dp, p + dp - camera.position, &triangles, 5);
      }
      color = color * (1.0 / 10.0);
      ret.push(gamma_correct(color));
    }
    eprintln!("finished rendering row {}", resolution_y - j);
  }
  ret
}

fn main() {
  let tlf = Vertex { position: Vec3::new3(-0.5, -0.5, 0.5),  color: Vec3::new3(0.0, 0.0, 1.0), .. Default::default() };
  let tlb = Vertex { position: Vec3::new3(-0.5, 0.5, 0.5),   color: Vec3::new3(0.0, 1.0, 1.0), .. Default::default() };
  let trf = Vertex { position: Vec3::new3(0.5, -0.5, 0.5),   color: Vec3::new3(1.0, 0.0, 1.0), .. Default::default() };
  let trb = Vertex { position: Vec3::new3(0.5, 0.5, 0.5),    color: Vec3::new3(1.0, 1.0, 1.0), .. Default::default() };
  let blf = Vertex { position: Vec3::new3(-0.5, -0.5, -0.5), color: Vec3::new3(0.0, 0.0, 0.0), .. Default::default() };
  let blb = Vertex { position: Vec3::new3(-0.5, 0.5, -0.5),  color: Vec3::new3(0.0, 1.0, 0.0), .. Default::default() };
  let brf = Vertex { position: Vec3::new3(0.5, -0.5, -0.5),  color: Vec3::new3(1.0, 0.0, 0.0), .. Default::default() };
  let brb = Vertex { position: Vec3::new3(0.5, 0.5, -0.5),   color: Vec3::new3(1.0, 1.0, 0.0), .. Default::default() };
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
    let sky_len = 5.0;
    vec![
      Triangle { a: Vertex { normal: front, .. tlf }, b: Vertex { normal: front, .. trf }, c: Vertex { normal: front, .. blf }, .. Default::default() },
      Triangle { a: Vertex { normal: front, .. blf }, b: Vertex { normal: front, .. brf }, c: Vertex { normal: front, .. trf }, .. Default::default() },
      Triangle { a: Vertex { normal: back,  .. tlb }, b: Vertex { normal: back,  .. trb }, c: Vertex { normal: back,  .. blb }, .. Default::default() },
      Triangle { a: Vertex { normal: back,  .. blb }, b: Vertex { normal: back,  .. brb }, c: Vertex { normal: back,  .. trb }, .. Default::default() },
      Triangle { a: Vertex { normal: left,  .. tlb }, b: Vertex { normal: left,  .. tlf }, c: Vertex { normal: left,  .. blf }, .. Default::default() },
      Triangle { a: Vertex { normal: left,  .. tlb }, b: Vertex { normal: left,  .. blb }, c: Vertex { normal: left,  .. blf }, .. Default::default() },
      Triangle { a: Vertex { normal: right, .. trb }, b: Vertex { normal: right, .. trf }, c: Vertex { normal: right, .. brf }, .. Default::default() },
      Triangle { a: Vertex { normal: right, .. trb }, b: Vertex { normal: right, .. brb }, c: Vertex { normal: right, .. brf }, .. Default::default() },
      Triangle { a: Vertex { normal: up,    .. tlf }, b: Vertex { normal: up,    .. trf }, c: Vertex { normal: up,    .. tlb }, .. Default::default() },
      Triangle { a: Vertex { normal: up,    .. trb }, b: Vertex { normal: up,    .. trf }, c: Vertex { normal: up,    .. tlb }, .. Default::default() },
      Triangle { a: Vertex { normal: down,  .. blf }, b: Vertex { normal: down,  .. brf }, c: Vertex { normal: down,  .. blb }, .. Default::default() },
      Triangle { a: Vertex { normal: down,  .. brb }, b: Vertex { normal: down,  .. brf }, c: Vertex { normal: down,  .. blb }, .. Default::default() },

      // Triangle {
      //   emitting: true,
      //   a: Vertex { position: Vec3::new3(-sky_len, -sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      //   b: Vertex { position: Vec3::new3(sky_len, -sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      //   c: Vertex { position: Vec3::new3(0.0, sky_len, 2.0), color: Vec3::new3(brightness, brightness, brightness), normal: down },
      // },
      Triangle {
        a: Vertex { position: Vec3::new3(0.0, sky_len, -1.0), color: Vec3::new3(0.2, 0.2, 0.2), normal: up },
        b: Vertex { position: Vec3::new3(sky_len * (0.75_f32.sqrt()), sky_len * -0.5, -1.0), color: Vec3::new3(0.2, 0.2, 0.2), normal: up },
        c: Vertex { position: Vec3::new3(sky_len * -(0.75_f32.sqrt()), sky_len * -0.5, -1.0), color: Vec3::new3(0.2, 0.2, 0.2), normal: up },
        matt: 1,
        .. Default::default()
      },
    ]
  };
  let horizontal_fov = 3.14 * 0.6;
  let camera = Camera {
    position: Vec3::new3(2.5, -5.0, 2.5),
    direction: Vec3::new3(-1.5, 3.0, -1.5),
    up: Vec3::new3(-1.0 / 3.0, 1.0 / 3.0, 1.0),
    // position: Vec3::new3(-3., 0., 0.),
    // direction: Vec3::new3(1., 0., 0.),
    // up: Vec3::new3(0., 0., 1.),
    // position: Vec3::new3(0.0, 0.0, 8.0),
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
