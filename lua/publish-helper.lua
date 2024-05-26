local ts = require'vim.treesitter'

local M = {}

function M.get_node_hl(bufnr, row, col)
  local buf_highlighter = ts.highlighter.active[bufnr]
  local hlid = 0
  local len = 1

  if buf_highlighter then
    buf_highlighter.tree:for_each_tree(function(tstree, tree)
      if not tstree then
        return
      end

      local root = tstree:root()
      local root_start_row, _, root_end_row, _ = root:range()

      local row0 = row - 1
      local col0 = col - 1

      -- Only worry about trees within the line range
      if root_start_row > row0 or root_end_row < row0 then
        return
      end

      local query = buf_highlighter:get_query(tree:lang())

      -- Some injected languages may not have highlight queries.
      if not query:query() then
        return
      end

      local iter = query:query():iter_captures(root, bufnr, row0, row0 + 1)

      for capture, node, _, _ in iter do
        if ts.is_in_node_range(node, row0, col0) then
          local name = query._query.captures[capture]
          if name == 'spell' or name == 'nospell' then
            goto skip
          end
          hlid = vim.fn.has('nvim-0.10') == 1
            and query:get_hl_from_capture(capture)
            or query.hl_cache[capture]
          if hlid ~= 0 then
            local _, _, end_row, end_col = ts.get_node_range(node)
            len = end_row > row0 and 0 or end_col - col0
          end
        end ::skip::
      end
    end)
  end

  if hlid == 0 and vim.b.current_syntax ~= nil then
    hlid = vim.fn.synID(row, col, 1)
  end

  return {hlid, len}
end

return M

