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

package de.markusbordihn.easynpc.data.editor;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public enum EditorType {
  NONE,
  ACTION_DATA,
  ACTION_DATA_ENTRY,
  DIALOG,
  DIALOG_BUTTON,
  DIALOG_TEXT;

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

  public ResourceLocation getId() {
    return new ResourceLocation(Constants.MOD_ID, this.name().toLowerCase() + "_editor");
  }

  public String getName() {
    return this.name().toLowerCase() + "_editor";
  }

  public Component getEditorTitle(final EasyNPC<?> easyNPC) {
    String translationKey = Constants.TEXT_CONFIG_PREFIX + this.name().toLowerCase() + ".title";
    return Component.translatable(translationKey, easyNPC.getEntity().getName().getString(20));
  }
}
