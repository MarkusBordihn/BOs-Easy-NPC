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

import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class YesNoDialogConfigurationScreen
    extends DialogConfigurationScreen<YesNoDialogConfigurationMenu> {

  protected EditBox mainDialogBox;
  protected EditBox yesDialogBox;
  protected EditBox noDialogBox;
  protected EditBox yesDialogButtonBox;
  protected EditBox noDialogButtonBox;
  protected Button saveDialogButton = null;
  protected Button cancelButton = null;

  public YesNoDialogConfigurationScreen(YesNoDialogConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.yesNoDialogButton.active = false;

    // Dialog
    this.mainDialogBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 60, 281, 20,
        Component.literal("Main Dialog Text"));
    this.mainDialogBox.setMaxLength(255);
    this.mainDialogBox.setValue(this.entity.getSimpleDialog());
    this.addRenderableWidget(this.mainDialogBox);

    // Dialog Buttons
    this.yesDialogButtonBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 85, 130, 20,
        Component.literal("Yes Dialog Button"));
    this.yesDialogButtonBox.setMaxLength(32);
    this.yesDialogButtonBox.setValue(this.entity.getYesDialogButton());
    this.addRenderableWidget(this.yesDialogButtonBox);

    this.noDialogButtonBox = new EditBox(this.font, this.leftPos + 158, this.topPos + 85, 130, 20,
        Component.literal("No Dialog Button"));
    this.noDialogButtonBox.setMaxLength(32);
    this.noDialogButtonBox.setValue(this.entity.getNoDialogButton());
    this.addRenderableWidget(this.noDialogButtonBox);

    // Yes Dialog
    this.yesDialogBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 130, 281, 20,
        Component.literal("Yes Path - Dialog Text"));
    this.yesDialogBox.setMaxLength(255);
    this.yesDialogBox.setValue(this.entity.getYesDialog());
    this.addRenderableWidget(this.yesDialogBox);

    // No Dialog
    this.noDialogBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 170, 281, 20,
        Component.literal("No Path - Dialog Text"));
    this.noDialogBox.setMaxLength(255);
    this.noDialogBox.setValue(this.entity.getNoDialog());
    this.addRenderableWidget(this.noDialogBox);

    // Save Button
    this.saveDialogButton = this.addRenderableWidget(
        menuButton(this.contentLeftPos + 26, this.bottomPos - 40, 80, "save", onPress -> {
          NetworkMessageHandler.saveYesNoDialog(uuid, this.mainDialogBox.getValue(),
              this.yesDialogBox.getValue(), this.noDialogBox.getValue(),
              this.yesDialogButtonBox.getValue(), this.noDialogButtonBox.getValue());
        }));

    // Chancel Button
    this.cancelButton = this.addRenderableWidget(
        menuButton(this.rightPos - 120, this.bottomPos - 40, 80, "cancel", onPress -> {
          this.closeScreen();
        }));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);
    guiGraphics.drawString(this.font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "question"), this.contentLeftPos,
        this.topPos + 50, Constants.FONT_COLOR_DEFAULT, false);

    guiGraphics.drawString(this.font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "yes_answer"), this.contentLeftPos,
        this.yesDialogBox.getY() - 12, Constants.FONT_COLOR_DEFAULT, false);

    guiGraphics.drawString(this.font,
        Component.translatable(Constants.TEXT_CONFIG_PREFIX + "no_answer"), this.contentLeftPos,
        this.noDialogBox.getY() - 12, Constants.FONT_COLOR_DEFAULT, false);
  }

}
