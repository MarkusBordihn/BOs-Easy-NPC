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

import de.markusbordihn.easynpc.client.screen.configuration.skin.EasyNPCButton;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkHandler;

@OnlyIn(Dist.CLIENT)
public class DialogConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  // Buttons
  protected Button basicDialogButton = null;
  protected Button yesNoDialogButton = null;

  public DialogConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Dialog Types
    this.basicDialogButton =
        this.addRenderableWidget(new EasyNPCButton(this.contentLeftPos, this.buttonTopPos, 80, 20,
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "basic_dialog"), onPress -> {
              NetworkHandler.openConfiguration(uuid, ConfigurationType.BASIC_DIALOG);
            }));
    this.yesNoDialogButton = this.addRenderableWidget(
        new EasyNPCButton(this.contentLeftPos + this.basicDialogButton.getWidth(), this.buttonTopPos, 80,
            20, Component.translatable(Constants.TEXT_CONFIG_PREFIX + "yes_no_dialog"), onPress -> {
              NetworkHandler.openConfiguration(uuid, ConfigurationType.YES_NO_DIALOG);
            }));

    // Default button stats
    this.basicDialogButton.active = true;
    this.yesNoDialogButton.active = true;
  }

}
