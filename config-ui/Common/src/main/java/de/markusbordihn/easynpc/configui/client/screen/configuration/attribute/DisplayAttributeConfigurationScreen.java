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

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DisplayAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final List<DisplayAttributeType> TIME_VISIBILITY_ATTRIBUTES =
      List.of(DisplayAttributeType.VISIBLE_AT_DAY, DisplayAttributeType.VISIBLE_AT_NIGHT);

  private static final List<DisplayAttributeType> GAMEMODE_VISIBILITY_ATTRIBUTES =
      List.of(
          DisplayAttributeType.VISIBLE_IN_STANDARD,
          DisplayAttributeType.VISIBLE_IN_CREATIVE,
          DisplayAttributeType.VISIBLE_IN_SPECTATOR);

  private static final List<DisplayAttributeType> SPECIAL_VISIBILITY_ATTRIBUTES =
      List.of(DisplayAttributeType.VISIBLE_TO_OWNER, DisplayAttributeType.VISIBLE_TO_TEAM);

  private static final long PREVIEW_DAY_TIME = 6000L;
  private static final long PREVIEW_NIGHT_TIME = 18000L;

  private final HashSet<Checkbox> visibilityCheckboxSet = new HashSet<>();

  private EditBox lightLevelBox;
  private Button lightLevelSaveButton;
  private EditBox opacityBox;
  private Button opacitySaveButton;
  private PreviewTime previewTime = PreviewTime.DAY;

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

    this.opacityBox =
        this.addRenderableWidget(
            new TextField(
                this.font,
                secondButtonRow + 100,
                this.buttonTopPos + 25,
                30,
                displayAttributeData.getDisplayIntAttribute(DisplayAttributeType.OPACITY),
                3));
    this.opacityBox.setResponder(
        value -> {
          if (this.opacitySaveButton != null) {
            this.opacitySaveButton.active =
                ValueUtils.isNumericValue(
                        value, DisplayAttributeType.MIN_OPACITY, DisplayAttributeType.MAX_OPACITY)
                    && ValueUtils.getIntValue(value)
                        != this.getEasyNPC()
                            .getEasyNPCDisplayAttributeData()
                            .getDisplayIntAttribute(DisplayAttributeType.OPACITY);
          }
        });
    this.opacitySaveButton =
        this.addRenderableWidget(
            new SaveButton(
                this.opacityBox.getX() + this.opacityBox.getWidth() + 2,
                this.opacityBox.getY() - 1,
                onPress -> {
                  int opacity = Integer.parseInt(this.opacityBox.getValue());
                  if (opacity >= DisplayAttributeType.MIN_OPACITY
                      && opacity <= DisplayAttributeType.MAX_OPACITY) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeDisplayAttribute(
                            this.getEasyNPCUUID(), DisplayAttributeType.OPACITY, opacity);
                  }
                }));
    this.opacitySaveButton.active = false;

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

    // Interaction behavior, independent of the master visibility toggle.
    this.addRenderableWidget(
        new Checkbox(
            firstButtonRow,
            checkboxTopPos,
            DisplayAttributeType.INTERACTION_WHEN_INVISIBLE.getAttributeName(),
            displayAttributeData.getDisplayBooleanAttribute(
                DisplayAttributeType.INTERACTION_WHEN_INVISIBLE),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .changeDisplayAttribute(
                        this.getEasyNPCUUID(),
                        DisplayAttributeType.INTERACTION_WHEN_INVISIBLE,
                        checkbox.selected())));

    // Preview to verify the configured visibility without changing the world time.
    this.addRenderableWidget(
        new SpinButton<>(
            secondButtonRow + 60,
            this.buttonTopPos + 215,
            100,
            16,
            new LinkedHashSet<>(List.of(PreviewTime.values())),
            this.previewTime,
            spinButton -> this.previewTime = spinButton.get()));
  }

  private boolean isVisibleInPreview() {
    return this.minecraft == null
        || this.minecraft.player == null
        || VisibilityHandler.isVisibleToPlayerAtDayTime(
            this.getEasyNPC(), this.minecraft.player, this.previewTime.dayTime);
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

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

    if (this.opacityBox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "opacity",
          this.opacityBox.getX() - 100,
          this.opacityBox.getY() + 4);
    }

    // Calculate section positions
    int timeSectionY = this.buttonTopPos + 65;
    int specialSectionY = this.buttonTopPos + 145;

    // Time visibility section header
    Text.drawConfigString(
        guiGraphics, this.font, "time_visibility_settings", firstButtonRow, timeSectionY, 0x555555);

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

    if (getEasyNPC() == null) {
      return;
    }

    if (!this.isVisibleInPreview()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "preview_hidden",
          secondButtonRow + 80,
          this.contentTopPos + 150,
          0x999999);
      return;
    }

    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        getEasyNPC(),
        EntityRenderConfig.guiScaled(this.leftPos + 265, this.contentTopPos + 160, 30),
        this.xMouse,
        this.yMouse);
  }

  private enum PreviewTime {
    DAY(PREVIEW_DAY_TIME),
    NIGHT(PREVIEW_NIGHT_TIME);

    private final long dayTime;

    PreviewTime(long dayTime) {
      this.dayTime = dayTime;
    }

    @Override
    public String toString() {
      return TextComponent.getTranslatedConfigText(
              "preview_time_" + this.name().toLowerCase(Locale.ROOT))
          .getString();
    }
  }
}
