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

package de.markusbordihn.easynpc.configui.client.screen.configuration.attribute;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.Arrays;
import java.util.HashSet;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DisplayAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final HashSet<DisplayAttributeType> TIME_VISIBILITY_ATTRIBUTES =
      new HashSet<>(
          Arrays.asList(
              DisplayAttributeType.VISIBLE_AT_DAY, DisplayAttributeType.VISIBLE_AT_NIGHT));

  private static final HashSet<DisplayAttributeType> GAMEMODE_VISIBILITY_ATTRIBUTES =
      new HashSet<>(
          Arrays.asList(
              DisplayAttributeType.VISIBLE_IN_CREATIVE,
              DisplayAttributeType.VISIBLE_IN_SPECTATOR,
              DisplayAttributeType.VISIBLE_IN_STANDARD));

  private static final HashSet<DisplayAttributeType> SPECIAL_VISIBILITY_ATTRIBUTES =
      new HashSet<>(
          Arrays.asList(
              DisplayAttributeType.VISIBLE_TO_OWNER, DisplayAttributeType.VISIBLE_TO_TEAM));

  private final HashSet<Checkbox> visibilityCheckboxSet = new HashSet<>();

  private EditBox lightLevelBox;
  private Button lightLevelSaveButton;

  public DisplayAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.displayAttributeButton.active = false;

    // Button rows
    int firstButtonRow = this.leftPos + 10;
    int secondButtonRow = this.leftPos + 160;

    // Attribute data
    DisplayAttributeDataCapable<?> displayAttributeData =
        this.getEasyNPC().getEasyNPCDisplayAttributeData();

    // Light Level
    this.lightLevelBox =
        this.addRenderableWidget(
            new TextField(
                this.font,
                firstButtonRow + 100,
                this.buttonTopPos + 25,
                20,
                displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL),
                2));
    this.lightLevelBox.setResponder(
        value -> {
          if (this.lightLevelSaveButton != null) {
            this.lightLevelSaveButton.active =
                ValueUtils.isNumericValue(value, 0, 15)
                    && ValueUtils.getIntValue(value)
                        != this.getEasyNPC()
                            .getEasyNPCDisplayAttributeData()
                            .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL);
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
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeDisplayAttribute(
                            this.getEasyNPCUUID(), DisplayAttributeType.LIGHT_LEVEL, lightLevel);
                  }
                }));
    this.lightLevelSaveButton.active = false;

    // Main is visible attribute
    Checkbox isVisibleCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                firstButtonRow,
                this.buttonTopPos + 45,
                DisplayAttributeType.VISIBLE.getAttributeName(),
                displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.VISIBLE),
                checkbox -> {
                  this.visibilityCheckboxSet.forEach(
                      visibilityCheckbox -> visibilityCheckbox.active = checkbox.selected());
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeDisplayAttribute(
                          this.getEasyNPCUUID(), DisplayAttributeType.VISIBLE, checkbox.selected());
                }));

    // Add time-based visibility attributes with header
    int checkboxTopPos = this.buttonTopPos + 65;
    int gamemodeCheckboxTopPos = checkboxTopPos;
    checkboxTopPos += 15;
    gamemodeCheckboxTopPos += 15;

    for (DisplayAttributeType displayAttributeType : TIME_VISIBILITY_ATTRIBUTES) {
      Checkbox visibilityCheckbox =
          new Checkbox(
              firstButtonRow,
              checkboxTopPos,
              displayAttributeType.getAttributeName(),
              displayAttributeData.getDisplayBooleanAttribute(displayAttributeType),
              checkbox ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeDisplayAttribute(
                          this.getEasyNPCUUID(), displayAttributeType, checkbox.selected()));
      visibilityCheckbox.active = isVisibleCheckbox.selected();
      this.visibilityCheckboxSet.add(this.addRenderableWidget(visibilityCheckbox));
      checkboxTopPos += 20;
    }

    // Add game mode visibility attributes with header
    for (DisplayAttributeType displayAttributeType : GAMEMODE_VISIBILITY_ATTRIBUTES) {
      Checkbox visibilityCheckbox =
          new Checkbox(
              secondButtonRow,
              gamemodeCheckboxTopPos,
              displayAttributeType.getAttributeName(),
              displayAttributeData.getDisplayBooleanAttribute(displayAttributeType),
              checkbox ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeDisplayAttribute(
                          this.getEasyNPCUUID(), displayAttributeType, checkbox.selected()));
      visibilityCheckbox.active = isVisibleCheckbox.selected();
      this.visibilityCheckboxSet.add(this.addRenderableWidget(visibilityCheckbox));
      gamemodeCheckboxTopPos += 20;
    }

    // Add special visibility attributes with header
    int specialSectionY = Math.max(checkboxTopPos, gamemodeCheckboxTopPos) + 5;
    checkboxTopPos = specialSectionY + 15;

    for (DisplayAttributeType displayAttributeType : SPECIAL_VISIBILITY_ATTRIBUTES) {
      Checkbox visibilityCheckbox =
          new Checkbox(
              firstButtonRow,
              checkboxTopPos,
              displayAttributeType.getAttributeName(),
              displayAttributeData.getDisplayBooleanAttribute(displayAttributeType),
              checkbox ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeDisplayAttribute(
                          this.getEasyNPCUUID(), displayAttributeType, checkbox.selected()));
      visibilityCheckbox.active = isVisibleCheckbox.selected();
      this.visibilityCheckboxSet.add(this.addRenderableWidget(visibilityCheckbox));
      checkboxTopPos += 20;
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    int firstButtonRow = this.leftPos + 10;
    int secondButtonRow = this.leftPos + 160;

    if (this.lightLevelBox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "light_level",
          this.lightLevelBox.getX() - 100,
          this.lightLevelBox.getY() + 4);
    }

    // Calculate section positions
    int timeSectionY = this.buttonTopPos + 65;
    int specialSectionY = this.buttonTopPos + 150;

    // Time visibility section header
    Text.drawConfigString(
        guiGraphics, this.font, "time_visibility_settings", firstButtonRow, timeSectionY, 0x555555);

    // Game mode visibility section header (jetzt in zweiter Spalte)
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "gamemode_visibility_settings",
        secondButtonRow,
        timeSectionY,
        0x555555);

    // Special visibility section header
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "special_visibility_settings",
        firstButtonRow,
        specialSectionY,
        0x555555);

    if (getEasyNPC() != null) {
      EntityConfigScreenRenderer.renderEntity(
          guiGraphics,
          getEasyNPC(),
          EntityRenderConfig.guiScaled(this.leftPos + 260, this.contentTopPos + 170, 30),
          this.xMouse,
          this.yMouse);
    }
  }
}
