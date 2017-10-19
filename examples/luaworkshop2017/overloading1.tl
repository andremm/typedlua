local function make_paths_from_tree (
  cfg:{"lua_modules_path":string,"lib_modules_path":string},
  tree:string|{"lua_dir":string,"lib_dir":string,"bin_dir":string}):
  (string, string, string)
  if type(tree) == "string" then
    return tree .. cfg.lua_modules_path,
           tree .. cfg.lib_modules_path,
           tree .. "/bin"
  else
    return tree.lua_dir, tree.lib_dir, tree.bin_dir
  end
end

