theme_set(theme_bw(base_size = 20))
mycss <- "
/* SliderInput color*/
.irs--shiny .irs-bar--single {
    border-radius: 8px 0 0 8px;
    background: purple;
    border-color: black;
}
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #fff;
    text-shadow: none;
    padding: 1px 3px;
    background-color: #800080;
    border-radius: 3px;
    font-size: 11px;
    line-height: 1.333;
}

/* Sidebar distance between icon and text*/
.fa-solid, .fas {
    font-weight: 900;
    margin-right: 10px;
}

/* Sidebar input coloring
.selectize-control.single .selectize-input, .selectize-control.single .selectize-input input {
    cursor: pointer;
    background: purple;
}*/

/* options */
.option {
  color: purple;
  background: white
}

.option.selected {
  color: white;
  background: purple
}


.selectize-input.focus {
    border-color: purple;
    box-shadow: none;
}

.form-control:focus {
    border-color: purple;
}

a {
    color: purple;
}
"