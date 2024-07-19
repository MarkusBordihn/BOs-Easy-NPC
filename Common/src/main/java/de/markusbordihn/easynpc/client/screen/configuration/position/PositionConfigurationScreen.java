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

package de.markusbordihn.easynpc.client.screen.configuration.position;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.message.NetworkMessageHandlerManager;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class PositionConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected Button defaultPositionButton;

  public PositionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Position Types
    int positionButtonWidth = 80;
    this.defaultPositionButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                positionButtonWidth,
                "default",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.DEFAULT_POSITION)));
  }

  @Override
  protected void renderScreenBg(GuiGraphics guiGraphics) {
    Graphics.blit(guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 40);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 203, topPos, 132, 0, 120, 40);
    Graphics.blit(
        guiGraphics, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 40, 0, 130, 210, 40);
    Graphics.blit(
        guiGraphics,
        Constants.TEXTURE_DEMO_BACKGROUND,
        leftPos + 203,
        topPos + 40,
        132,
        130,
        120,
        40);
  }
}
