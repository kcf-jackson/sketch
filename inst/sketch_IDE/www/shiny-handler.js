document.addEventListener("keyup", function(e) {
  console.log(e);
  if (e.key === "Enter" && (e.altKey || e.metaKey || e.ctrlKey)) {
    e.preventDefault();
    Shiny.setInputValue("editor_text", editor.getValue());
    $("#transpile").click();
  }
});

Shiny.addCustomMessageHandler("replace_textarea",
  function(message) {
    console.log(message);
    if (message.status == "error") {
         $('textarea#r_console').val(message.msg);
    } else {
         $('textarea#r_console').val("");
         $('iframe#tgt-render').attr("src", message.msg);
    }
  }
);
