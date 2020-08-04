#lang silicate

component C0
  port x : in integer
  port y : out integer
  y = x
end

interface I0
  port x : in integer
  port y : out integer
end

component C1
  port i : I0
  i.y = i.x
end

component C2
  port i[2] : I0
  i[0].y = i[0].x
  i[1].y = i[1].x
end

interface I1
  port i[2] : I0
end

component C3
  port j[2] : I1
  j[0].i[0].y = j[0].i[0].x
  j[0].i[1].y = j[0].i[1].x
  j[1].i[0].y = j[1].i[0].x
  j[1].i[1].y = j[1].i[1].x
end

component C4
  port x : out integer
  x = 10
end

component C5
  port x : out integer
  constant k = 10
  x = k
end

component C6
  port x : out integer
  constant k = 10
  x = k + 1
end

component C7
  port x : in integer
  port y : in integer
  port z : out integer
  z = x + y
end

component C8
  port x : in integer
  port y : in integer
  port z : in integer
  port u : in integer
  port v : out integer
  v = x * y + z * u
end

component C9
  port x : in integer
  port y : in integer
  port z : in integer
  port u : in integer
  port v : out integer
  let xy = x * y
  let zu = z * u
  v = xy + zu
end

interface I2
  port x : in integer
end

component C10
  port i[3] : I2
  port y : in integer
  port z : out integer
  z = i[y].x
end

component C11(N : integer)
  port x : in integer
  port y : out integer
  y = x * N
end

component C12
  port x : in integer
  port y : out integer
  instance c = C11(10)
  c.x = x
  y = c.y
end

component C13
  port x0 : in integer
  port x1 : in integer
  port y : out integer
  instance c[2] = C11(10)
  c[0].x = x0
  c[1].x = x1
  y = c[0].y + c[1].y
end

component C14
  port i : splice I0
  y = x
end

component C15
  port j : splice I1
  i[0].y = i[0].x
  i[1].y = i[1].x
end

interface I3
  port i : splice I0
end

component C16
  port j : I3
  j.y = j.x
end

component C17
  port j : splice I3
  y = x
end

component C18
  port x : in integer
  port y : in integer
  port z : out integer
  z = if x > y then x else y
end
