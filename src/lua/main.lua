-- Source: https://gist.github.com/ayourtch/752460

-- Andrew Yourtchenko - Programming Challange from Erann Gat:
-- http://www.flownet.com/ron/papers/lisp-java/
-- Given a list of words and a list of phone numbers, find all the ways that
-- each phone number can be expressed as a list of words.

-- No batteries included. We have to define our own split() function.
function string:split(sep)
  local sep, fields = sep or ":", {}
  local pattern = string.format("([^%s]+)", sep)
  self:gsub(pattern, function(c) fields[#fields+1] = c end)
  return fields
end

-- how to replace the letters with the numbers
repl = { e = 0, jnq = 1, rwx = 2, dsy = 3, ft = 4, am = 5, civ = 6, bku = 7, lop = 8, ghz = 9 }

-- generate the table for patterns and the target replacements, ignore the double quote

repl2 = { ['["]'] = '' }
for k,n in pairs(repl) do
  repl2["[" .. k .. string.upper(k) .. "]"] = tostring(n)
end

-- read the dictionary
dict = {}
f = io.open("dictionary.txt")
while true do
  line = f:read()
  if line then
    table.insert(dict, line)
  else
    break
  end
end
f:close()

-- make a working 2D table, the indices being the first two digits of the number transposed
-- and the data element being a tuple - remainder of the number and the word itself
repl3 = {}

for i,v in ipairs(dict) do
  local s = v
  for k,n in pairs(repl2) do
    s = s:gsub(k,n)
  end
  local c1 = s:sub(1,1)
  local c2 = s:sub(2,2)
  local val = { s:sub(3), v }
  if repl3[c1] then
    if repl3[c1][c2] then
      table.insert(repl3[c1][c2], val)
    else
      repl3[c1][c2] = { val }
    end
  else
    repl3[c1] = {}
    repl3[c1][c2] = { val }
  end
end


function translate(orig_str, str, accum, has_digit)
  local c1 = str:sub(1,1)
  local c2 = str:sub(2,2)
  if (str == "") then --  or (c1 == nil and c2 == nil) then
    -- success, print the original number and its encoding
    print(orig_str .. ": " .. table.concat(accum, " "))
    return
  end
  if repl3[c1] and repl3[c1][c2] then
    local count = 0
    for i, v in ipairs(repl3[c1][c2]) do
      if(v[1] == str:sub(3,v[1]:len()+2)) then
        -- the remainder of the candidate number matched, explore this branch
        count = count + 1
        accum[#accum+1] = v[2]
        translate(orig_str, str:sub(2+v[1]:len()+1), accum, false)
        accum[#accum] = nil
      end
    end
    if count == 0 and not has_digit then
      -- try with a single digit if there were no words that matched
      accum[#accum+1] = c1
      translate(orig_str, str:sub(2), accum, true)
      accum[#accum] = nil
    end
  else
    if not has_digit then
      -- try a digit since there was no word found
      local acc2 = accum;
      accum[#accum+1] = c1
      translate(orig_str, str:sub(2), accum, true)
      accum[#accum] = nil
    end
  end
end

f = io.open("input.txt")
while true do
  line = f:read()
  if line then
    translate(line, line:gsub("[^0-9]",""), {})
  else
    break
  end
end
f:close()

