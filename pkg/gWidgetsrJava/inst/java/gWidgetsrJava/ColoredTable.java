/*
 *  XNap Commons
 *
 *  Copyright (C) 2005  Felix Berger
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

//package org.xnap.commons.gui;


package gWidgetsrJava;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JTable;
import javax.swing.UIManager;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

/**
 * A table that uses a different background color for every other row in the 
 * table.
 * TODO provide setter for colors
 * TODO lighten other color for dark themes
 */
public class ColoredTable extends JTable
{
	
	private Color oddColor;
	private Color evenColor;

	public ColoredTable(TableModel dm, TableColumnModel cm)
	{
		super(dm, cm);
		updateColors();
	}
	
	public ColoredTable(TableModel dm)
	{
		super(dm);
		updateColors();
	}
	
    public ColoredTable()
	{
		updateColors();
	}

	public Color getBackgroundColor(int row)
	{
		return (row % 2 == 1) ? oddColor : evenColor;
	}

	private void updateColors()
	{
		evenColor = UIManager.getColor("Table.background");
		//Color odd = UIManager.getColor("Label.background");	
		oddColor = (evenColor != null) 
			? new Color(Math.max((int)(evenColor.getRed()   * 0.9), 0), 
						Math.max((int)(evenColor.getGreen() * 0.9), 0),
						Math.max((int)(evenColor.getBlue()  * 0.9), 0))
			: null;
	}

	public void updateUI() 
	{
		super.updateUI();
		updateColors();
	}		

	/**
	 * The idea for this implementation stems from LimeWire's
	 * JMultilineToolTipTreeTable, thanks guys.
	 */
	@Override
	public Component prepareRenderer(TableCellRenderer renderer, int row,
									 int column)
	{
		Component c = super.prepareRenderer(renderer, row, column);

		boolean isSelected = isCellSelected(row, column);
		boolean rowIsAnchor = 
			(selectionModel.getAnchorSelectionIndex() == row);
		boolean colIsAnchor =
			(columnModel.getSelectionModel().getAnchorSelectionIndex()
			 == column);
		boolean hasFocus = (rowIsAnchor && colIsAnchor) && hasFocus();
		if (!isSelected && !hasFocus) {
			c.setBackground(getBackgroundColor(row));
		}
		return c;
	}
	
}
