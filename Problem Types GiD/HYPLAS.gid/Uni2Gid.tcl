# UniToGid  -*- TCL -*- Created: ramsan Sep-1996, Modified: ramsan Sep-1996


#----------------------Uni2Gid ALONE---------------------------
# the next line restarts using wish \
	exec wish "$0" "$@"

set Uni2GidRunningAlone 0
# if { ![info exists GIDDEFAULT] } {
#     set Uni2GidRunningAlone 1
#     set tcl_interactive 0
#     set tk_strictMotif 1

#     if { [catch { set GIDDEFAULT $env(GIDDEFAULT)}]!=1 } {
# 	set GIDDEFAULTTCL $GIDDEFAULT
# 	catch { set GIDDEFAULTTCL $env(GIDDEFAULTTCL) }
# 	set auto_path "$GIDDEFAULTTCL"
# 	source $GIDDEFAULTTCL/init.tcl
# 	source $GIDDEFAULTTCL/tk.tcl
#     }
# 	wm withdraw .
# }

#  LOOK AT THE END OF THE FILE !!!
#-------------------------------------------------------------------

proc ArrangeMatFrame { num w MatList} {

    for {set i 0} {$i <= 6} {incr i} {
	if { $i > $num && [winfo exists $w.f$i] } {
	    destroy $w.f$i
	} elseif { $i <= $num && ![winfo exists $w.f$i] } {
	    frame $w.f$i
	    label $w.f$i.l -text "Convert material num $i to:"
	    menubutton $w.f$i.mb -text [lindex $MatList 0] -menu $w.f$i.mb.m \
		    -relief raised -width 15
	    menu $w.f$i.mb.m
	    foreach j $MatList {
		$w.f$i.mb.m add command -label $j -command "$w.f$i.mb conf -text $j"
	    }
	    pack $w.f$i.l $w.f$i.mb -side left
	    pack $w.f$i -side top
	}
    }
}

proc ArrangeMatAndTempFrame { w MatList} {

    set names { cast mould contact }
    set num 3
    for {set i 1} {$i <= $num} {incr i} {
	if { $i > $num && [winfo exists $w.f$i] } {
	    destroy $w.f$i
	} elseif { $i <= $num && ![winfo exists $w.f$i] } {
	    frame $w.f$i
	    set name [lindex $names [expr $i-1]]
	    label $w.f$i.l -text "Convert $name material num to:"
	    menubutton $w.f$i.mb -text [lindex $MatList 0] -menu $w.f$i.mb.m \
		    -relief raised -width 15
	    menu $w.f$i.mb.m
	    foreach j $MatList {
		$w.f$i.mb.m add command -label $j -command "$w.f$i.mb conf -text $j"
	    }
	    if { $i < 3 } {
		label $w.f$i.l2 -text "Initial temper.:"
		entry $w.f$i.e -relief sunken -width 15
		$w.f$i.e ins end 0.0
		pack $w.f$i.l $w.f$i.mb $w.f$i.l2 $w.f$i.e -side left
	    } else { pack $w.f$i.l $w.f$i.mb -side left }
	    pack $w.f$i -side top
	}
    }
}

proc LoadUniFileToGid { type } {
    global Uni2GidRunningAlone HavePressedOK

    if { ![info exists Uni2GidRunningAlone] } {
	set Uni2GidRunningAlone 0
    }

    if { [.central.s info Mesh] } {
	set aa [mkDialogN .modal  \
		"-text \"There is already a mesh. Erase it?\"  \
		-aspect 250 -justify left" { Cancel Cancel }  { OK OK }]
	if { $aa == "OK" } {
	    .central.s process escape escape escape escape generation generate \
		    erase escape escape
	} else { return }
    }
    set fileName [Browser-ramR file read . {Read UNIGRAPHICS}]
    update idletasks
    if { $fileName == "" } { return }

    set MatList [.central.s info materials]

    set w .wunitogid0
    catch { destroy $w}
    toplevel $w
    wm title $w "UNIGRAPHICS to GID"
    wm iconname $w "Uni2Gid"
    set pare [winfo parent $w]
    set posx [expr [winfo x $pare]+[winfo width $pare ]/4]
    set posy [expr [winfo y $pare]+[winfo height $pare ]/4]
    wm geometry $w [join "+$posx + $posy" ""]

    frame $w.f0

    if { $type == "mechanical" } {
	label $w.f0.l -text "Number of materials:"
	menubutton $w.f0.mb -text 0 -menu $w.f0.mb.m -relief raised -width 3
	menu $w.f0.mb.m
	for {set i 0} {$i <= 6} {incr i} {
	    $w.f0.mb.m add command -label "  $i  " -command "$w.f0.mb conf -text $i ; \
		    ArrangeMatFrame $i $w.f1 \"$MatList\""
	}
	pack $w.f0.l $w.f0.mb -side left
	
	frame $w.f1 -relief ridge -bd 4
	ArrangeMatFrame 0 $w.f1 $MatList
    } elseif { $type == "thermal" } {
	label $w.f0.l -text "Number of materials: 3"
	pack $w.f0.l -side left
	
	frame $w.f1 -relief ridge -bd 4
	ArrangeMatAndTempFrame $w.f1 $MatList
    } else {
	mkDialogN .modal  \
		"-text \"Only mechanical or thermal problems accepted\"  \
		-aspect 250 -justify left" { OK OK }
	destroy $w
	return
    }

    frame $w.f2
    button $w.f2.ok -text OK -command "set HavePressedOK 1"
    button $w.f2.c -text Cancel -command "set HavePressedOK 0"
    pack $w.f2.ok $w.f2.c -side left

    pack $w.f0 $w.f1 $w.f2 -side top

    tkwait visibility $w
    grab  $w
    tkwait variable HavePressedOK
    if { $HavePressedOK == 0 } { 
	destroy $w
	return 
    }
    set MatTransfList(0) ""
    if { $type == "mechanical" } { set ini 0 } else { set ini 1 }
    for {set i $ini} {$i <= 6} {incr i} {
	if { ![winfo exists $w.f1.f$i] } { break }
	set mat [$w.f1.f$i.mb cget -text]
	set num 1
	foreach j $MatList {
	    if { $mat == $j } { break }
	    incr num
	}
	set MatTransfList($i) $num
	if { $type == "thermal" && [winfo exists $w.f1.f$i.e] } {
	    set TemperatureTransfList($i) [$w.f1.f$i.e get]
	}
    }
    destroy $w

    if { $type == "mechanical" } {
	set fail [UniToGid $fileName .wunitogid MatTransfList]
	if { $fail } {
	    destroy .wunitogid
	    return
	}
    } else {
	UniThermalToGid $fileName .wunitogid MatTransfList TemperatureTransfList
    }
    
    tkwait visib .wunitogid
    set DefaultDialogWrowser \
	    [.central.s info variables DialogWrowser]
    if { $DefaultDialogWrowser == 1 } {
	.central.s process escape escape escape escape utilities variables \
		DialogWrowser 0 escape
    }
    .central.s process escape escape escape escape files batchfile tmp-uni.batch

    if { $DefaultDialogWrowser == 1 } {
	.central.s process escape escape escape escape utilities variables \
		DialogWrowser 1 escape
    }
    after idle file delete tmp-uni.msh tmp-uni.batch
}

proc UniToGid { name {w .wunitogid } {MatTransfListName ""}} {
    global Uni2GidRunningAlone

    upvar $MatTransfListName MatTransfList

    if { ![info exists Uni2GidRunningAlone] } {
	set Uni2GidRunningAlone 0
    }
    catch { destroy $w}
    
    toplevel $w
    wm title $w "UNIGRAPHICS mechanical to GID"
    wm iconname $w "Uni2MGid"
    if { $Uni2GidRunningAlone == 1 } {
	wm geom $w +500+500
    } else {
	set pare [winfo parent $w]
	set posx [expr [winfo x $pare]+[winfo width $pare ]/4]
	set posy [expr [winfo y $pare]+[winfo height $pare ]/4]
	wm geometry $w [join "+$posx + $posy" ""]
    }
    
    set root tmp-uni
    set from [open $name r]


    gets $from buffer
    if { [regexp {TITLE[ ]*NEUTRAL} $buffer]==0 } {
	tk_dialogRAM $w.tmpwin error "This is not a neutral UNIGRAPHICS file" error 0 OK
	close $from
	destroy $w
	return 1
    }

    set to [open $root.msh w]
    set tobatch [open $root.batch w]
    set ElemType tetrahedra

    puts $tobatch "escape escape escape escape data problemdata Mechanical 3D"


    label $w.l1 -text "Converting to $ElemType mesh ..."
    pack $w.l1 -side top
    update idletasks


    gets $from buffer
    set NumLocAxes [lindex $buffer 1]

    label $w.l15 -text "Writing $NumLocAxes local axes"
    pack $w.l15 -side top
    update idletasks

    if { $NumLocAxes > 0 } {
	puts $tobatch "escape escape escape escape data localaxes define"
	for {set i 1} {$i <= $NumLocAxes} {incr i} {
	    gets $from buffer
	    gets $from buffer2
	    set axesname [string trim [string range $buffer 0 7]]
	    puts $tobatch "LA$axesname 3PointsXZ"
	    puts $tobatch "[string range $buffer 72 79] [string range $buffer2 0 7] \
		    [string range $buffer2 8 15]"
	    puts $tobatch "[string range $buffer2 40 47] \
		    [string range $buffer2 48 55] [string range $buffer2 56 63]"
	    puts $tobatch "[string range $buffer2 16 23] \
		    [string range $buffer2 24 31] [string range $buffer2 32 39]"
	}
	puts $tobatch "escape escape escape escape"
    }
    gets $from buffer
    set NumPoints [lindex $buffer 1]

    label $w.l2 -text "Writing $NumPoints points"
    pack $w.l2 -side top
    update idletasks

    if { $ElemType == "tetrahedra" } {
	puts $to "MESH dimension = 3 ElemType Tetrahedra  Nnode = 4"
    } else {
	puts $to "MESH dimension = 3 ElemType Triangle  Nnode = 3"
    }
    puts $to "COORDINATES"

    for {set i 1} {$i <= $NumPoints} {incr i} {
	gets $from buffer
	puts $to "[string range $buffer 0 7] [string range $buffer 40 47]\
		[string range $buffer 48 55] [string range $buffer 56 63]"
    }
    gets $from buffer
    set NumElems [lindex $buffer 2]

    label $w.l3 -text "Writing $NumElems elements"
    pack $w.l3 -side top
    update idletasks

    puts $to "end coordinates\nelements"

    for {set i 1} {$i <= $NumElems} {incr i} {
	gets $from buffer
	set matnum [string range $buffer 24 31]
	set matnum [string trim $matnum " "]
	if { [info exists MatTransfList($matnum)] } {
	    set MatNewNum $MatTransfList($matnum)
	} else { set MatNewNum "" }
	
	# WARNING: renumbering elements numbers to begin with 1
	puts $to "$i [string range $buffer 32 39]\
		[string range $buffer 40 47] [string range $buffer 48 55]\
		[string range $buffer 56 63] $MatNewNum"
    }
    puts $to "end elements"

    close $to
    puts $tobatch "layer new base escape "
    puts $tobatch "escape escape escape escape files meshread $root.msh"

    set num 4
    label $w.l$num -text "written elements..."
    pack $w.l$num -side top
    update idletasks
    incr num

    gets $from buffer

    set NumForces 0
    set NumDespla 0
    # 0: none ; 1: force; 2: displacement
    set LastIsWhat 0
    while { ![eof $from] } {
	gets $from buffer
	if { [string range $buffer 0 7] == "LOADCASE" } { continue}
	gets $from buffer2
	set type [string trimright [string range $buffer 0 15] " "]
	if { $type == "FORCE"} {
	    if { $LastIsWhat != 1 } {
		puts $tobatch "escape escape escape escape data conditions \
			assign Point-Load"
	    }
	    puts $tobatch "change [string range $buffer 56 63] \
		    [string range $buffer 64 71] \
		    [string range $buffer 72 79] [string range $buffer 24 31]"
	    set LastIsWhat 1
	    incr NumForces
	} elseif { $type == "DISPLACEMENT"} {
	    if { $LastIsWhat != 2 } {
		puts $tobatch "escape escape escape escape data conditions assign \
			Point-Constraints"
	    }
	    set LocalAxesName LA[string trim [string range $buffer 40 47] " "]
	    if { $LocalAxesName == "LA1" } { set LocalAxesName "-GLOBAL-" }
	    for {set i 0} {$i <= 3} {incr i} {
		set val($i) [string trim [string range $buffer [expr 56+$i*8] \
			[expr 63+$i*8]] " "]
		if { $val($i) == "" } {
		    set val($i) 0.0
		    set ival($i) 0
		} elseif { $val($i) == "0.000000" } {
		    set val($i) 0.0
		    set ival($i) 1
		} else { set ival($i) 1}
	    }
	    puts $tobatch "change $LocalAxesName $ival(0) $val(0) $ival(1) $val(1) $ival(2) \
		     $val(2) 0 0.0 [string range $buffer 24 31]"
	    set LastIsWhat 2
	    incr NumDespla
	}
    }
    puts $tobatch "escape escape escape escape"

    label $w.l$num -text "written $NumForces forces"
    pack $w.l$num -side top
    incr num
    label $w.l$num -text "written $NumDespla prescribed displacements"
    pack $w.l$num -side top
    update idletasks
    incr num

    close $from
    close $tobatch
    label $w.l$num -text "-DONE- (Use batch file: $root.batch)"
    if { $Uni2GidRunningAlone == 0 } {
	$w.l$num conf -text "-DONE-"
    }
    pack $w.l$num -side top
    button $w.b2 -text OK -command "destroy $w"
    pack $w.b2 -side bottom
    return 0
}

proc UniThermalToGid { name {w .wunitogid } {MatTransfListName ""} \
	{TemperatureTransfListName ""}} {
    global Uni2GidRunningAlone GIDDEFAULT

    upvar $MatTransfListName MatTransfList
    upvar $TemperatureTransfListName TemperatureTransfList

    if { ![info exists Uni2GidRunningAlone] } {
	set Uni2GidRunningAlone 0
    }
    catch { destroy $w}
    
    toplevel $w
    wm title $w "UNIGRAPHICS thermal to GID"
    wm iconname $w "UniT2Gid"
    if { $Uni2GidRunningAlone == 1 } {
	wm geom $w +500+500
    } else {
	set pare [winfo parent $w]
	set posx [expr [winfo x $pare]+[winfo width $pare ]/4]
	set posy [expr [winfo y $pare]+[winfo height $pare ]/4]
	wm geometry $w [join "+$posx + $posy" ""]
    }
    
    set root tmp-uni

    set tobatch [open $root.batch w]
    set ElemType tetrahedra

    puts $tobatch "escape escape escape escape data problemdata Thermal 3D"


    label $w.l1 -text "Converting to $ElemType mesh ..."
    pack $w.l1 -side top
    update idletasks

    set MatCastNum $MatTransfList(1)
    set MatMouldNum $MatTransfList(2)
    set MatInterNum $MatTransfList(3)
    
#     set res [exec echo \
# 	    "$name\n$root.msh\n$MatCastNum\n$MatMouldNum\n$MatInterNum" \
# 	    | $GIDDEFAULT/vulcan2.gid/interfaceUNIT2GID]

#     if { ![regexp {.*npoin=[ \t]*([0-9]+).*nelem=[ \t]*([0-9]+).*Contact-elements=[ \t]*([0-9]+)} $res trash NumPoints NumElems NumContactElems] } {
#}

    set res [exec $GIDDEFAULT/vulcan2.gid/UniT2GID $name $root.msh $MatCastNum \
	    $MatMouldNum $MatInterNum]

    if { ![regexp {.*NumNodes=[ \t]*([0-9]+).*NumElems=[ \t]*([0-9]+).*NumContacts=[ \t]*([0-9]+)} $res trash NumPoints NumElems NumContactElems] } {
	mkDialogN .modal  \
		"-text \"Error reading data\"  \
		-aspect 250 -justify left" { OK OK }
	close $tobatch
	destroy $w
	return
    }


    label $w.l12 -text "Writing $NumPoints points\n$NumElems tetrahedra\
	    \n$NumContactElems contact elements"
    pack $w.l12 -side top
    update idletasks

    puts $tobatch "layer new base escape "
    puts $tobatch "escape escape escape escape files meshread $root.msh"

    set num 4
    label $w.l$num -text "written mesh..."
    pack $w.l$num -side top
    update idletasks
    incr num

    set FastSelection [.central.s info variables FastSelection]
    .central.s process escape escape escape escape utilities variables \
	    FastSelection 1 escape

    puts $tobatch "escape utilities variables FastSelection 1 escape escape"
    puts $tobatch "layer new cast-elems new mould-elems new contact-elems"
    puts $tobatch "material cast-elems $MatCastNum"
    puts $tobatch "material mould-elems $MatMouldNum"
    puts $tobatch "material contact-elems $MatInterNum"
    puts $tobatch "new cast-nodes new mould-nodes"
    puts $tobatch "freeze mould-elems freeze contact-elems"
    puts $tobatch "entities cast-nodes NodesOfElms 1:10000000 escape"
    puts $tobatch "freeze cast-elems unfreeze mould-elems"
    puts $tobatch "entities mould-nodes NodesOfElms 1:10000000 escape"
    puts $tobatch "unfreeze cast-elems unfreeze contact-elems"

    puts $tobatch "freeze mould-nodes"
    puts $tobatch "escape escape escape escape data cond \
	    assign Initial_Temp-(points) change $TemperatureTransfList(1)"
    puts $tobatch "1:10000000 escape"

    puts $tobatch "layer unfreeze mould-nodes freeze cast-nodes escape"
    puts $tobatch "assign Initial_Temp-(points) change $TemperatureTransfList(2)"
    puts $tobatch "1:10000000 escape escape escape"
    puts $tobatch "layer unfreeze cast-nodes escape"
    puts $tobatch "escape utilities variables FastSelection $FastSelection escape"

    close $tobatch
    label $w.l$num -text "-DONE- (Use batch file: $root.batch)"
    if { $Uni2GidRunningAlone == 0 } {
	$w.l$num conf -text "-DONE-"
    }
    pack $w.l$num -side top

    button $w.b2 -text OK -command "destroy $w"
    pack $w.b2 -side bottom

}


#----------------------HELP Uni2Gid ALONE---------------------------
if { $Uni2GidRunningAlone == 1 } {
    if { $argc != 1 } { 
	error "usage: Uni2Gid UnigrphicsFile " 
    }
    UniToGid [lindex $argv 0]
}
