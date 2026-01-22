let resolve_path path =
  let prefix = "@engine/" in
  if String.length path >= String.length prefix && String.sub path 0 (String.length prefix) = prefix then
    "engine/" ^ (String.sub path (String.length prefix) (String.length path - String.length prefix))
  else
    path
