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
