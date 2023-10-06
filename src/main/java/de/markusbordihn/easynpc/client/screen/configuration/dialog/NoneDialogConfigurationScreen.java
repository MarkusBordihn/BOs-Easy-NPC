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

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Checkbox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.menu.configuration.dialog.NoneDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class NoneDialogConfigurationScreen
    extends DialogConfigurationScreen<NoneDialogConfigurationMenu> {

  // Buttons
  protected Checkbox noneDialogCheckbox;

  // Text
  private List<FormattedCharSequence> textComponents = Collections.emptyList();
  protected int numberOfTextLines = 1;

  // Cache
  private static DialogType formerDialogType = DialogType.BASIC;

  public NoneDialogConfigurationScreen(NoneDialogConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  public static void setFormerDialogType(DialogType dialogType) {
    formerDialogType = dialogType;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.noneDialogButton.active = false;

    // Cache former dialog Type
    setFormerDialogType(this.entity.getDialogType());

    // None Dialog Checkbox
    this.noneDialogCheckbox =
        this.addRenderableWidget(new Checkbox(this.contentLeftPos + 75, this.topPos + 170, 20, 20,
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "disable_dialog_checkbox")
                .withStyle(ChatFormatting.WHITE),
            entity.getDialogType() == DialogType.NONE) {
          @Override
          public void onPress() {
            super.onPress();
            if (this.selected()) {
              NetworkMessageHandler.changeDialogType(uuid, DialogType.NONE);
            } else {
              NetworkMessageHandler.changeDialogType(uuid,
                  formerDialogType != null && formerDialogType != DialogType.NONE ? formerDialogType
                      : DialogType.BASIC);
            }
          }
        });

    // Pre-format text
    this.textComponents = this.font.split(
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "disable_dialog_text"),
        this.imageWidth - 20);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        guiGraphics.drawString(this.font, formattedCharSequence, leftPos + 15,
            topPos + 60 + (line * (font.lineHeight + 2)), Constants.FONT_COLOR_DEFAULT, false);
      }
    }
  }
}
