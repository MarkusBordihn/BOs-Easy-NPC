/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.data.editor;

import de.markusbordihn.easynpc.Constants;
import java.util.Locale;
import net.minecraft.resources.ResourceLocation;

public enum EditorType {
  NONE(false),
  ACTION_DATA,
  ACTION_DATA_ENTRY,
  CONDITION_DATA,
  CONDITION_DATA_ENTRY,
  DIALOG,
  DIALOG_BUTTON,
  DIALOG_OPTIONS,
  DIALOG_TEXT,
  FACTION,
  FACTIONS,
  TRADING_OFFER_ACTION(false);

  private final boolean hasMenu;
  private final String editorName = this.name().toLowerCase(Locale.ROOT) + "_editor";
  private final ResourceLocation id =
      ResourceLocation.fromNamespaceAndPath(Constants.MOD_ID, this.editorName);

  EditorType() {
    this.hasMenu = true;
  }

  EditorType(boolean hasMenu) {
    this.hasMenu = hasMenu;
  }

  public static EditorType get(String editorType) {
    if (editorType == null || editorType.isEmpty()) {
      return EditorType.NONE;
    }

    try {
      return EditorType.valueOf(editorType);
    } catch (IllegalArgumentException e) {
      return EditorType.NONE;
    }
  }

  public boolean hasMenu() {
    return this.hasMenu;
  }

  public ResourceLocation getId() {
    return this.id;
  }

  public String getName() {
    return this.editorName;
  }
}
