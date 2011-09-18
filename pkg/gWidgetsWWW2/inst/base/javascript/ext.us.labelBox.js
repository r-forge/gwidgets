// code from glabel
// From extjs.com??? (Not sure, sorry)

Ext.ux.labelBox = Ext.extend(Ext.BoxComponent, {
    value: null,
    initComponent:function() {
	Ext.ux.labelBox.superclass.initComponent.call(this);
        },
    onRender:function(ct, position) {
        this.el = document.createElement("span");
        this.el.id = this.getId();
	this.el.innerHTML = this.value;
        Ext.ux.labelBox.superclass.onRender.call(this,ct,position); 
        }, 
    getValue: function() { 
        return this.value; 
        }, 
    setValue: function(value) { 
        this.value = value; 
        document.getElementById(this.id).innerHTML = value; 
        } 
    });
Ext.reg("labelbox", Ext.ux.labelBox);
