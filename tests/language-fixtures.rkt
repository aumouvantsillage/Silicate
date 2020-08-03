#lang silicate

component Cab
  port a : in integer
  port b : out integer
  b = a
end

interface Iab
  port a : in integer
  port b : out integer
end

component CIab
  port i : Iab
  i.b = i.a
end

component CIab2
  port i[2] : Iab
  i[0].b = i[0].a
  i[1].b = i[1].a
end

interface IIab2
  port i[2] : Iab
end

component CIIab2
  port j[2] : IIab2
  j[0].i[0].b = j[0].i[0].a
  j[0].i[1].b = j[0].i[1].a
  j[1].i[0].b = j[1].i[0].a
  j[1].i[1].b = j[1].i[1].a
end

component Cal
  port a : out integer
  a = 10
end

component Cac
  port a : out integer
  constant k = 10
  a = k
end

component Cas
  port a : out integer
  constant k = 10
  a = k + 1
end
