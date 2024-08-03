/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.configuration;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.Screen;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.Collections;
import java.util.List;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class ConfigurationScreen<T extends EasyNPCMenu> extends Screen<T> {

  protected Button homeButton = null;
  protected int buttonLeftPos;
  protected int buttonTopPos;
  protected int contentLeftPos;
  protected int contentTopPos;
  protected int numberOfDescriptionTextLines = 1;
  protected List<FormattedCharSequence> descriptionTextComponents = Collections.emptyList();

  public ConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.showCloseButton = false;
  }

  protected void renderDescriptionText(
      GuiGraphics guiGraphics, int descriptionLeft, int descriptionTop) {
    if (!this.descriptionTextComponents.isEmpty() && this.numberOfDescriptionTextLines > 0) {
      for (int line = 0; line < this.numberOfDescriptionTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.descriptionTextComponents.get(line);
        Text.drawString(
            guiGraphics,
            this.font,
            formattedCharSequence,
            descriptionLeft,
            descriptionTop + (line * (font.lineHeight + 2)));
      }
    }
  }

  protected void setDescriptionText(String textId) {
    setDescriptionText(Component.translatable(Constants.TEXT_CONFIG_PREFIX + textId));
  }

  protected void setDescriptionText(Component component) {
    this.descriptionTextComponents = this.font.split(component, this.imageWidth - 20);
    this.numberOfDescriptionTextLines = this.descriptionTextComponents.size();
  }

  @Override
  public void init() {
    super.init();

    // Core Positions
    this.buttonLeftPos = this.leftPos + 13;
    this.buttonTopPos = this.topPos + 3;
    this.contentLeftPos = this.leftPos + 7;
    this.contentTopPos = this.topPos + 23;

    // Home Button
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 3,
                this.buttonTopPos,
                10,
                "<",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.MAIN)));
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int x, int y) {
    // Render Title if not in compact mode
    if (!this.compactMode) {
      Text.drawString(
          guiGraphics,
          this.font,
          this.title,
          this.leftPos + this.titleLabelX,
          this.topPos + this.titleLabelY,
          4210752);
    }
  }

  public void showMainScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.MAIN);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    // Capture ALT + left arrow key to navigate back to main screen
    if (keyCode == 263 && hasAltDown()) {
      showMainScreen();
      return true;
    }
    return super.keyPressed(keyCode, unused1, unused2);
  }
}
