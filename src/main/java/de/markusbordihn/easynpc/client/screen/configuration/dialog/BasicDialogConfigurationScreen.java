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

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class BasicDialogConfigurationScreen
    extends DialogConfigurationScreen<BasicDialogConfigurationMenu> {

  protected EditBox dialogBox;
  protected Button saveDialogButton = null;
  protected Button cancelButton = null;

  public BasicDialogConfigurationScreen(BasicDialogConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicDialogButton.active = false;
    this.yesNoDialogButton.active = true;

    // Dialog
    this.dialogBox = new EditBox(this.font, this.contentLeftPos, this.topPos + 60, 281, 20,
        new TranslatableComponent("Dialog"));
    this.dialogBox.setMaxLength(255);
    this.dialogBox.setValue(this.entity.getDialog());
    this.addRenderableWidget(this.dialogBox);

    // Save Button
    this.saveDialogButton = this.addRenderableWidget(
        menuButton(this.contentLeftPos + 26, this.bottomPos - 40, 80, "save", onPress -> {
          NetworkMessage.saveBasicDialog(uuid, this.dialogBox.getValue());
        }));

    // Chancel Button
    this.cancelButton = this.addRenderableWidget(
        menuButton(this.rightPos - 120, this.bottomPos - 40, 80, "cancel", onPress -> {
          this.closeScreen();
        }));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.font.draw(poseStack,
        new TranslatableComponent(Constants.TEXT_CONFIG_PREFIX + "dialog_text"),
        this.contentLeftPos, this.topPos + 50f, 4210752);
  }

}
