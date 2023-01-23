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

package de.markusbordihn.easynpc.client.screen.configuration;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.menu.configuration.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class BasicDialogConfigurationScreen
    extends DialogConfigurationScreen<BasicDialogConfigurationMenu> {

  private EditBox dialogBox;
  private Button saveDialogButton = null;

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
    this.dialogBox = new EditBox(this.font, this.leftPos + 7, this.topPos + 70, 261, 20,
        new TranslatableComponent("Dialog"));
    this.dialogBox.setMaxLength(255);
    this.dialogBox.setValue(this.entity.getDialog());
    this.addRenderableWidget(this.dialogBox);

    this.saveDialogButton = this.addRenderableWidget(new Button(this.leftPos + 6, this.topPos + 93,
        80, 20, new TranslatableComponent("Save"), onPress -> {
          log.info("Save dialog ...");
          NetworkHandler.saveDialog(uuid, DialogType.BASIC, this.dialogBox.getValue());
        }));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.font.draw(poseStack, new TextComponent("Basic Dialog"), this.leftPos + 7f,
        this.topPos + 55f, 4210752);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69) {
      return super.keyPressed(keyCode, unused1, unused2);
    } else if (keyCode == 257 || keyCode == 335) {
      return true;
    } else {
      return true;
    }
  }

}
