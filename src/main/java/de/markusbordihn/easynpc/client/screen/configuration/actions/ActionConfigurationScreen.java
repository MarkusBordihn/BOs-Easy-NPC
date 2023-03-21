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

package de.markusbordihn.easynpc.client.screen.configuration.actions;

import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.action.ActionData;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessage;

@OnlyIn(Dist.CLIENT)
public class ActionConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  // Buttons
  protected Button basicActionButton = null;
  protected Button dialogActionButton = null;
  protected Button yesNoActionButton = null;

  public ActionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public EditBox actionEditBox(int left, int top, ActionData actionData) {
    EditBox editBox = new EditBox(this.font, left, top, 256, 16, new TextComponent(""));
    editBox.setMaxLength(255);
    editBox.setValue(actionData != null ? actionData.getAction() : "");
    return editBox;
  }

  @Override
  public void init() {
    super.init();

    // Action Types
    this.basicActionButton = this.addRenderableWidget(
        menuButton(this.buttonLeftPos, this.buttonTopPos, 80, "basic_actions", onPress -> {
          NetworkMessage.openConfiguration(uuid, ConfigurationType.BASIC_ACTION);
        }));
    this.dialogActionButton = this.addRenderableWidget(
        menuButton(this.basicActionButton.x + this.basicActionButton.getWidth(), this.buttonTopPos,
            80, "dialog_actions", onPress -> {
              NetworkMessage.openConfiguration(uuid, ConfigurationType.DIALOG_ACTION);
            }));

    // Default button stats
    this.basicActionButton.active = true;
    this.dialogActionButton.active = true;
  }

}
