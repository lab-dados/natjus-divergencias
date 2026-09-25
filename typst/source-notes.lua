-- Source and note paragraphs under tables and figures, for Typst output.
--
-- The manuscript marks them as fenced divs with custom-style="Fonte" and
-- custom-style="Notas", which Pandoc maps to Word paragraph styles. The
-- Typst writer drops the attribute and emits a plain #block, so here the
-- divs become calls to #fonte-legenda and #notas-legenda, defined in the
-- layout profile (typst/perfis/labdados-wp.typ). Other formats are left
-- untouched.
--
-- The divs follow their table or figure as siblings. In Typst that lets a
-- page break fall between the two, leaving "Fonte:" alone at the top of a
-- page, and it keeps the float from being placed as a unit. So when a div
-- directly follows a float, it is moved inside the float's content, after
-- the table or image (the caption is a separate slot of the float). The
-- filter runs at Quarto's pre-quarto entry point, where Quarto has already
-- turned tables and images with a cross-reference id into FloatRefTarget
-- custom nodes.

-- "Autor" is the paragraph block of one author on the first page; unlike
-- the two note styles it never follows a float, so it stays in place.
local functions = { Fonte = "fonte-legenda", Notas = "notas-legenda", Autor = "autor" }

local function note_style(el)
  if el.t ~= "Div" then return nil end
  local style = el.attributes["custom-style"]
  if style ~= nil and functions[style] ~= nil then return style end
  return nil
end

local function as_raw(el, style)
  local blocks = pandoc.List({ pandoc.RawBlock("typst", "#" .. functions[style] .. "[") })
  blocks:extend(el.content)
  blocks:insert(pandoc.RawBlock("typst", "]"))
  return blocks
end

local function is_float(el)
  return el.t == "Div" and el.attributes ~= nil
    and el.attributes.__quarto_custom == "true"
    and el.attributes.__quarto_custom_type == "FloatRefTarget"
end

-- Two passes. User filters cannot resolve a custom node from its Div, but a
-- FloatRefTarget handler receives both the node data and the Div, whose
-- __quarto_custom_id is visible in the block list as well. The first pass
-- collects the note divs that follow each float under that id and removes
-- them; the second appends them to the float's content.
local pending = {}

local function collect_notes(blocks)
  local out = pandoc.Blocks({})
  local i = 1
  while i <= #blocks do
    local el = blocks[i]
    out:insert(el)
    if is_float(el) then
      local extra = pandoc.Blocks({})
      local j = i + 1
      while j <= #blocks and note_style(blocks[j]) ~= nil and note_style(blocks[j]) ~= "Autor" do
        extra:extend(as_raw(blocks[j], note_style(blocks[j])))
        j = j + 1
      end
      if #extra > 0 then
        pending[el.attributes.__quarto_custom_id] = extra
      end
      i = j
    else
      i = i + 1
    end
  end
  return out
end

-- Column widths, as percentages of the text width, for the tables whose
-- content-sized columns come out wrong: Typst shrinks every column of a
-- crowded table, and a column holding a long unhyphenated word then
-- overflows into its neighbour. The keys are the cross-reference ids; the
-- Word output keeps the widths its own configuration derives.
local widths = {
  ["tbl-local-national"] = { 11, 14, 18, 7, 18, 20, 12 },
  ["tbl-variables"] = { 13, 14, 25, 27, 21 },
  ["tbl-model-1-2-origins"] = { 20, 10, 12, 18, 20, 20 },
  ["tbl-model-1-1-origins"] = { 17, 9, 11, 16, 16, 16, 15 },
  ["tbl-conitec-differences"] = { 24, 14, 10, 10, 10, 19, 13 },
}

local function set_widths(float)
  local w = widths[float.identifier]
  local tbl = float.content
  if tbl ~= nil and tbl.t == nil then tbl = tbl[1] end
  if w == nil or tbl == nil or tbl.t ~= "Table" or #tbl.colspecs ~= #w then
    return
  end
  local specs = pandoc.List({})
  for i, spec in ipairs(tbl.colspecs) do
    specs:insert({ spec[1], w[i] / 100 })
  end
  tbl.colspecs = specs
end

local function append_notes(float, node)
  set_widths(float)
  local extra = pending[node.attributes.__quarto_custom_id]
  if extra == nil then return float end
  local content = float.content
  if content == nil then
    content = pandoc.Blocks({})
  elseif content.t ~= nil then
    content = pandoc.Blocks({ content })
  else
    content = pandoc.Blocks(content)
  end
  content:extend(extra)
  float.content = content
  return float
end

if quarto.doc.is_format("typst") then
  return {
    { Blocks = collect_notes },
    { FloatRefTarget = append_notes },
    -- notes that follow no float keep their own block
    { Div = function(el)
        local style = note_style(el)
        if style == nil then return nil end
        return as_raw(el, style)
      end },
  }
end
return {}
