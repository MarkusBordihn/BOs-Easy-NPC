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

package de.markusbordihn.easynpc.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.menu.configuration.attribute.DisplayAttributeConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DisplayAttributeConfigurationScreen
    extends AttributeConfigurationScreen<DisplayAttributeConfigurationMenu> {

  private EditBox lightLevelBox;
  private Button lightLevelSaveButton;

  public DisplayAttributeConfigurationScreen(
      DisplayAttributeConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.displayAttributeButton.active = false;

    int firstButtonRow = this.leftPos + 10;

    // Attribute data
    AttributeData<?> attributeData = this.easyNPC.getEasyNPCAttributeData();

    // Light Level
    this.lightLevelBox =
        this.addRenderableWidget(
            new TextField(
                this.font,
                firstButtonRow + 100,
                this.buttonTopPos + 25,
                20,
                attributeData.getAttributeLightLevel(),
                2));
    this.lightLevelBox.setResponder(
        value -> {
          if (this.lightLevelSaveButton != null) {
            this.lightLevelSaveButton.active = value != null && !value.isEmpty();
          }
        });
    this.lightLevelSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.lightLevelBox.getX() + this.lightLevelBox.getWidth() + 2,
                this.lightLevelBox.getY() - 1,
                onPress -> {
                  int lightLevel = Integer.parseInt(this.lightLevelBox.getValue());
                  if (lightLevel >= 0 && lightLevel <= 15) {
                    NetworkMessageHandler.entityAttributeChange(
                        uuid, EntityAttribute.LIGHT_LEVEL, lightLevel);
                  }
                }));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    if (this.lightLevelBox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "light_level",
          this.lightLevelBox.getX() - 100,
          this.lightLevelBox.getY() + 4);
    }
  }
}
