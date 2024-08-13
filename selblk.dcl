selblk : dialog {
    label = "Edit Block Attributes";
    : column {
        : boxed_column {
            label = "Attributes";
            : list_box {
                key = "attlist";
                label = "Select Attribute:";
                width = 20;
                height = 15;
            }
        }
        : boxed_column {
            label = "Edit Attribute";
            : row {
                : text_part {
                    key = "attlabel";
                    label = "Attribute:";
                }
                : text_part {
                    key = "currentvalue";
                    label = "Current Value:";
                }
                : edit_box {
                    key = "attvalue";
                    label = "New Value:";
                    edit_width = 25;
                }
            }
        }
    }
    : row {
        : button {
            key = "apply";
            label = "Apply Changes";
            is_default = true;
        }
        : button {
            key = "cancel";
            label = "Cancel";
        }
    }
}

// Aaron Cendejas was here and did all of the above ^^^