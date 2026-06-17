BEGIN { p = 50 }
{
  gsub("L", "-")
  gsub("R", "+")
  p = (p + $0) % 100
  if (p < 0) p = p + 100
  if (p == 0) c++
}
END { print c }
