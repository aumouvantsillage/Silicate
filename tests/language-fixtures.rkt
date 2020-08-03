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
