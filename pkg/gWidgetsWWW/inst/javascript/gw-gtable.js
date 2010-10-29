// column formats for gtable. Based on types
// text is red or black depending
gtableNumeric = function(val) { 
    return '<span style="color:red">' + val + '</span>';
}
    
gtableInteger = function(val) { 
    return '<span style="color:red">' + val + '</span>';
};

gtableLogical = function(val) { 
    return '<span style="color:black">' + val + '</span>';
    };

gtableIcon = function(val) { 
    return '<img src="' + val + '">';
}
