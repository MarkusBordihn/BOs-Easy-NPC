/**
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

package de.markusbordihn.easynpc.client.screen.configuration.dialog;

import java.util.Collections;
import java.util.List;

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class BasicDialogConfigurationScreen
    extends DialogConfigurationScreen<BasicDialogConfigurationMenu> {

  // Buttons
  protected EditBox dialogBox;
  protected Button saveDialogButton = null;
  protected Button cancelButton = null;

  // Text
  private List<FormattedCharSequence> textComponents = Collections.emptyList();
  protected int numberOfTextLines = 1;

  public BasicDialogConfigurationScreen(BasicDialogConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicDialogButton.active = false;

    // Dialog
    this.dialogBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 60, 281, 20,
        Component.translatable("Dialog"));
    this.dialogBox.setMaxLength(255);
    this.dialogBox.setValue(this.entity.getSimpleDialog());
    this.addRenderableWidget(this.dialogBox);

    // Save Button
    this.saveDialogButton = this.addRenderableWidget(
        menuButton(this.contentLeftPos + 26, this.bottomPos - 40, 80, "save", onPress -> {
          NetworkMessageHandler.saveBasicDialog(uuid, this.dialogBox.getValue());
        }));

    // Chancel Button
    this.cancelButton = this.addRenderableWidget(
        menuButton(this.rightPos - 120, this.bottomPos - 40, 80, "cancel", onPress -> {
          this.closeScreen();
        }));

    // Pre-format text
    this.textComponents =
        this.font.split(Component.translatable(Constants.TEXT_CONFIG_PREFIX + "dialog_placeholder"),
            this.imageWidth - 20);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    guiGraphics.drawString(this.font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "dialog_text"), this.contentLeftPos,
        this.topPos + 50, Constants.FONT_COLOR_DEFAULT, false);

    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        guiGraphics.drawString(this.font, formattedCharSequence, leftPos + 15,
            topPos + 100 + (line * (font.lineHeight + 2)), Constants.FONT_COLOR_DEFAULT, false);
      }
    }
  }

}
