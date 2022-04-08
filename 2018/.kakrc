hook global WinSetOption filetype=ocaml %{
  set-option buffer indentwidth 2 
}

hook global WinCreate .*\.(txt|md) %{
  set-option buffer autowrap_column 80
  set-option buffer autowrap_format_paragraph true
}

hook global WinCreate .*/(input|example.*)\.txt %{
    set buffer readonly true
    remove-highlighter buffer/wrap_-word
    remove-highlighter buffer/show-whitespaces
}
