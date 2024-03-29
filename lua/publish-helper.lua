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

      local iter = query:query():iter_captures(root, buf_highlighter.bufnr,
                                               row0, row0 + 1)

      for capture, node, _ in iter do
        local hl = query.hl_cache[capture]

        if hl and ts.is_in_node_range(node, row0, col0) then
          local c = query._query.captures[capture]
          if c ~= nil and c ~= '' then
            local cur_hlid = 0
            cur_hlid = vim.fn.hlID(c)
            if cur_hlid ~= 0 then
              local _, _, end_row, end_col = ts.get_node_range(node)
              hlid = cur_hlid
              if end_row > row0 then
                len = 0
              else
                len = end_col - col0
              end
            end
          end
        end
      end
    end, true)
  end

  if hlid == 0 and vim.b.current_syntax ~= nil then
    for _, id in ipairs(vim.fn.synstack(row, col)) do
      hlid = vim.fn.hlID(vim.fn.synIDattr(vim.fn.synIDtrans(id), "name"))
    end
  end

  return {hlid, len}
end

return M

