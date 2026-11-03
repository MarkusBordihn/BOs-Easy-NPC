/*
 * Copyright 2026 Markus Bordihn
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
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BossBarHandler;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.function.IntSupplier;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.BossEvent;
import net.minecraft.world.entity.player.Inventory;

public class BossBarAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final int LABEL_OFFSET = 10;
  private static final int WIDGET_OFFSET = 120;
  private static final int LABEL_TOP_OFFSET = 4;
  private static final int SHOW_BOSS_BAR_TOP_OFFSET = 25;
  private static final int COLOR_TOP_OFFSET = 50;
  private static final int STYLE_TOP_OFFSET = 70;
  private static final int RANGE_TOP_OFFSET = 90;
  private static final int AUTO_HIDE_TOP_OFFSET = 110;
  private static final int AUTO_HIDE_HINT_TOP_OFFSET = 135;
  private static final int AUTO_HIDE_HINT_WIDTH = 300;
  private static final int HINT_COLOR = 0x555555;

  private EditBox rangeBox;
  private EditBox autoHideBox;

  public BossBarAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    DisplayAttributeDataCapable<?> displayAttributeData =
        this.getEasyNPC().getEasyNPCDisplayAttributeData();
    int widgetLeft = this.leftPos + WIDGET_OFFSET;

    this.addRenderableWidget(
        new Checkbox(
            this.leftPos + LABEL_OFFSET,
            this.buttonTopPos + SHOW_BOSS_BAR_TOP_OFFSET,
            DisplayAttributeType.SHOW_BOSS_BAR.getAttributeName(),
            displayAttributeData.getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR),
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .changeDisplayAttribute(
                        this.getEasyNPCUUID(),
                        DisplayAttributeType.SHOW_BOSS_BAR,
                        checkbox.selected())));

    this.addRenderableWidget(
        new SpinButton<>(
            widgetLeft,
            this.buttonTopPos + COLOR_TOP_OFFSET,
            105,
            16,
            new LinkedHashSet<>(List.of(BossEvent.BossBarColor.values())),
            BossBarHandler.getBossBarColor(displayAttributeData),
            spinButton ->
                NetworkMessageHandlerManager.getServerHandler()
                    .changeDisplayAttribute(
                        this.getEasyNPCUUID(),
                        DisplayAttributeType.BOSS_BAR_COLOR,
                        spinButton.get().name())));

    this.addRenderableWidget(
        new SpinButton<>(
            widgetLeft,
            this.buttonTopPos + STYLE_TOP_OFFSET,
            105,
            16,
            new LinkedHashSet<>(List.of(BossEvent.BossBarOverlay.values())),
            BossBarHandler.getBossBarStyle(displayAttributeData),
            spinButton ->
                NetworkMessageHandlerManager.getServerHandler()
                    .changeDisplayAttribute(
                        this.getEasyNPCUUID(),
                        DisplayAttributeType.BOSS_BAR_STYLE,
                        spinButton.get().name())));

    this.rangeBox =
        this.addIntegerField(
            this.buttonTopPos + RANGE_TOP_OFFSET,
            DisplayAttributeType.BOSS_BAR_RANGE,
            DisplayAttributeType.MIN_BOSS_BAR_RANGE,
            DisplayAttributeType.MAX_BOSS_BAR_RANGE,
            () ->
                BossBarHandler.getBossBarRange(
                    this.getEasyNPC().getEasyNPCDisplayAttributeData()));

    this.autoHideBox =
        this.addIntegerField(
            this.buttonTopPos + AUTO_HIDE_TOP_OFFSET,
            DisplayAttributeType.BOSS_BAR_AUTO_HIDE,
            DisplayAttributeType.MIN_BOSS_BAR_AUTO_HIDE_SECONDS,
            DisplayAttributeType.MAX_BOSS_BAR_AUTO_HIDE_SECONDS,
            () ->
                this.getEasyNPC()
                    .getEasyNPCDisplayAttributeData()
                    .getDisplayIntAttribute(DisplayAttributeType.BOSS_BAR_AUTO_HIDE));
  }

  private EditBox addIntegerField(
      int top,
      DisplayAttributeType attributeType,
      int minimum,
      int maximum,
      IntSupplier savedValueSupplier) {
    EditBox editBox =
        this.addRenderableWidget(
            new TextField(
                this.font,
                this.leftPos + WIDGET_OFFSET,
                top,
                30,
                savedValueSupplier.getAsInt(),
                String.valueOf(maximum).length()));
    Button saveButton =
        this.addRenderableWidget(
            new SaveButton(
                editBox.getX() + editBox.getWidth() + 2,
                editBox.getY() - 1,
                onPress -> {
                  if (ValueUtils.isNumericValue(editBox.getValue(), minimum, maximum)) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeDisplayAttribute(
                            this.getEasyNPCUUID(),
                            attributeType,
                            ValueUtils.getIntValue(editBox.getValue()));
                  }
                }));
    saveButton.active = false;
    editBox.setResponder(
        value ->
            saveButton.active =
                ValueUtils.isNumericValue(value, minimum, maximum)
                    && ValueUtils.getIntValue(value) != savedValueSupplier.getAsInt());
    return editBox;
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    if (this.rangeBox == null || this.autoHideBox == null) {
      return;
    }

    int labelLeft = this.leftPos + LABEL_OFFSET;
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "boss_bar_color",
        labelLeft,
        this.buttonTopPos + COLOR_TOP_OFFSET + LABEL_TOP_OFFSET);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "boss_bar_style",
        labelLeft,
        this.buttonTopPos + STYLE_TOP_OFFSET + LABEL_TOP_OFFSET);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "boss_bar_range_blocks",
        labelLeft,
        this.rangeBox.getY() + LABEL_TOP_OFFSET);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "boss_bar_auto_hide",
        labelLeft,
        this.autoHideBox.getY() + LABEL_TOP_OFFSET);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "boss_bar_auto_hide.always_visible",
        this.autoHideBox.getX() + this.autoHideBox.getWidth() + 30,
        this.autoHideBox.getY() + LABEL_TOP_OFFSET,
        HINT_COLOR);

    int hintTop = this.buttonTopPos + AUTO_HIDE_HINT_TOP_OFFSET;
    for (FormattedCharSequence line :
        this.font.split(
            TextComponent.getTranslatedConfigText("boss_bar_auto_hide.hint"),
            AUTO_HIDE_HINT_WIDTH)) {
      guiGraphics.text(this.font, line, labelLeft, hintTop, HINT_COLOR, false);
      hintTop += this.font.lineHeight + 2;
    }
  }
}
