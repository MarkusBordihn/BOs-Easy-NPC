/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.LinkedHashMap;
import java.util.Map;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class SetSkinEntry extends ActionEntryWidget {

  private static final int ROW_WIDTH = 285;

  private final Map<String, String> skinNames = new LinkedHashMap<>();
  private SpinButton<String> skinSelectionButton;

  public SetSkinEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    if (this.screen.getEasyNPC() instanceof EasyNPCBase<?> easyNPCBase) {
      for (Enum<?> variant : easyNPCBase.getSkinVariantTypes()) {
        this.skinNames.put(variant.name(), variant.name());
      }
    }
    AdditionalScreenData.getServerSkins(this.screen.getAdditionalScreenData().getData())
        .forEach((skinUUID, skinName) -> this.skinNames.put(skinUUID.toString(), skinName));

    String storedSkin =
        this.hasActionData(ActionDataType.SET_SKIN) ? this.actionDataEntry.command() : "";
    if (!storedSkin.isEmpty()) {
      this.skinNames.putIfAbsent(storedSkin, storedSkin);
    }
    if (this.skinNames.isEmpty()) {
      return;
    }

    this.skinSelectionButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                editorTop + 20,
                ROW_WIDTH,
                16,
                this.skinNames.keySet(),
                storedSkin.isEmpty() ? this.skinNames.keySet().iterator().next() : storedSkin,
                button -> {}));
    this.skinSelectionButton.setLabelProvider(
        skinReference -> TextComponent.getText(this.skinNames.get(skinReference)));
  }

  private String getSkin() {
    return this.skinSelectionButton != null ? this.skinSelectionButton.get() : "";
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.skin",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.skin.hint",
        editorLeft + 2,
        editorTop + 42,
        Constants.FONT_COLOR_GRAY);
  }

  @Override
  public boolean isValid() {
    return !this.getSkin().isEmpty();
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(ActionDataType.SET_SKIN)
        || !this.getSkin().equals(this.actionDataEntry.command());
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.SET_SKIN, this.getSkin());
  }
}
