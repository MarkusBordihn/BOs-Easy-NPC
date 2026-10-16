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

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.components.AddButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.EditButton;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.components.SaveButton;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectBox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributes;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class MiscAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  private static final List<InteractionAttributeType> SHOULDER_OFFSET_ATTRIBUTES =
      List.of(
          InteractionAttributeType.SHOULDER_OFFSET_X,
          InteractionAttributeType.SHOULDER_OFFSET_Y,
          InteractionAttributeType.SHOULDER_OFFSET_Z);
  private static final int SHOULDER_OFFSET_MAX_LENGTH = 6;

  private final Map<InteractionAttributeType, EditBox> shoulderOffsetBoxes =
      new EnumMap<>(InteractionAttributeType.class);
  private final Map<InteractionAttributeType, Button> shoulderOffsetSaveButtons =
      new EnumMap<>(InteractionAttributeType.class);
  protected SelectBox<String> factionSelectBox;
  protected Button factionEditButton;
  protected Button newFactionButton;
  private Checkbox sitOnShoulderCheckbox;
  private Button shoulderOffsetResetButton;
  private int shoulderSectionTop;

  public MiscAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private boolean hasFactionSelected() {
    return this.factionSelectBox != null
        && this.factionSelectBox.getSelectedValue() != null
        && !this.factionSelectBox.getSelectedValue().isEmpty();
  }

  private InteractionAttributes getInteractionAttributes() {
    return this.getEasyNPC()
        .getEasyNPCAttributeData()
        .getEntityAttributes()
        .getInteractionAttributes();
  }

  private double getSavedShoulderOffset(InteractionAttributeType attributeType) {
    InteractionAttributes interactionAttributes = this.getInteractionAttributes();
    return switch (attributeType) {
      case SHOULDER_OFFSET_X -> interactionAttributes.shoulderOffsetX();
      case SHOULDER_OFFSET_Y -> interactionAttributes.shoulderOffsetY();
      default -> interactionAttributes.shoulderOffsetZ();
    };
  }

  private boolean isChangedShoulderOffset(InteractionAttributeType attributeType, String value) {
    return ValueUtils.isDoubleValue(
            value,
            -InteractionAttributes.MAX_SHOULDER_OFFSET,
            InteractionAttributes.MAX_SHOULDER_OFFSET)
        && Double.parseDouble(value) != this.getSavedShoulderOffset(attributeType);
  }

  private void refreshShoulderOffsetWidgets() {
    boolean canSitOnShoulder = this.sitOnShoulderCheckbox.selected();
    for (InteractionAttributeType attributeType : SHOULDER_OFFSET_ATTRIBUTES) {
      EditBox shoulderOffsetBox = this.shoulderOffsetBoxes.get(attributeType);
      shoulderOffsetBox.setEditable(canSitOnShoulder);
      this.shoulderOffsetSaveButtons.get(attributeType).active =
          canSitOnShoulder
              && this.isChangedShoulderOffset(attributeType, shoulderOffsetBox.getValue());
    }
    this.shoulderOffsetResetButton.active = canSitOnShoulder;
  }

  private void addShoulderOffsetField(InteractionAttributeType attributeType, int top) {
    EditBox shoulderOffsetBox =
        this.addRenderableWidget(
            new TextField(
                this.font,
                this.contentLeftPos + 100,
                top,
                40,
                String.valueOf(this.getSavedShoulderOffset(attributeType)),
                SHOULDER_OFFSET_MAX_LENGTH));
    Button shoulderOffsetSaveButton =
        this.addRenderableWidget(
            new SaveButton(
                shoulderOffsetBox.getX() + shoulderOffsetBox.getWidth() + 2,
                top - 1,
                onPress -> {
                  if (this.isChangedShoulderOffset(attributeType, shoulderOffsetBox.getValue())) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .interactionAttributeChange(
                            this.getEasyNPCUUID(),
                            attributeType,
                            Double.parseDouble(shoulderOffsetBox.getValue()));
                  }
                }));
    shoulderOffsetSaveButton.active = false;
    shoulderOffsetBox.setResponder(
        value ->
            shoulderOffsetSaveButton.active =
                this.sitOnShoulderCheckbox.selected()
                    && this.isChangedShoulderOffset(attributeType, value));
    this.shoulderOffsetBoxes.put(attributeType, shoulderOffsetBox);
    this.shoulderOffsetSaveButtons.put(attributeType, shoulderOffsetSaveButton);
  }

  private void resetShoulderOffset() {
    for (InteractionAttributeType attributeType : SHOULDER_OFFSET_ATTRIBUTES) {
      NetworkMessageHandlerManager.getServerHandler()
          .interactionAttributeChange(this.getEasyNPCUUID(), attributeType, 0.0D);
      this.shoulderOffsetBoxes.get(attributeType).setValue(String.valueOf(0.0D));
      this.shoulderOffsetSaveButtons.get(attributeType).active = false;
    }
  }

  private void initShoulderSection() {
    this.shoulderSectionTop = this.contentTopPos + 50;

    this.sitOnShoulderCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 10,
                this.shoulderSectionTop + 12,
                InteractionAttributeType.CAN_SIT_ON_SHOULDER.getAttributeName(),
                this.getInteractionAttributes().canSitOnShoulder(),
                checkbox -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .interactionAttributeChange(
                          this.getEasyNPCUUID(),
                          InteractionAttributeType.CAN_SIT_ON_SHOULDER,
                          checkbox.selected());
                  this.refreshShoulderOffsetWidgets();
                }));
    this.addRenderableWidget(
        new HelpIcon(
            this.contentLeftPos + 225,
            this.shoulderSectionTop + 14,
            "can_sit_on_shoulder.tooltip"));

    int shoulderOffsetTop = this.shoulderSectionTop + 34;
    for (InteractionAttributeType attributeType : SHOULDER_OFFSET_ATTRIBUTES) {
      this.addShoulderOffsetField(attributeType, shoulderOffsetTop);
      shoulderOffsetTop += 20;
    }

    this.shoulderOffsetResetButton =
        this.addRenderableWidget(
            new TextButton(
                this.contentLeftPos + 175,
                this.shoulderSectionTop + 53,
                50,
                "reset",
                onPress -> this.resetShoulderOffset()));
    this.addRenderableWidget(
        new HelpIcon(
            this.contentLeftPos + 225, this.shoulderSectionTop + 37, "shoulder_offset.tooltip"));
    this.refreshShoulderOffsetWidgets();
  }

  @Override
  public void init() {
    super.init();

    this.miscAttributeButton.active = false;

    int factionEntriesTop = this.contentTopPos + 20;
    int factionEntriesSecondColumn = this.contentLeftPos + 100;

    String savedFactionName =
        this.getAdditionalScreenData()
            .getData()
            .getString(FactionDataCapable.DATA_FACTION_NAME_TAG)
            .orElse("");
    List<SelectOption<String>> factionOptions = new ArrayList<>();
    factionOptions.add(SelectOption.of("-", ""));
    for (String availableFactionName :
        AdditionalScreenData.getFactionData(this.getAdditionalScreenData().getData()).keySet()) {
      factionOptions.add(SelectOption.of(availableFactionName));
    }

    this.factionSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                factionEntriesSecondColumn,
                factionEntriesTop,
                125,
                16,
                factionOptions,
                selectedFactionName -> {
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeFaction(this.getEasyNPCUUID(), selectedFactionName);
                  if (this.factionEditButton != null) {
                    this.factionEditButton.active = this.hasFactionSelected();
                  }
                }));
    this.factionSelectBox.selectByValue(savedFactionName);

    this.factionEditButton =
        this.addRenderableWidget(
            new EditButton(
                this.factionSelectBox.getX() + this.factionSelectBox.getWidth() + 5,
                factionEntriesTop,
                onPress -> {
                  if (this.hasFactionSelected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .openFactionEditor(
                            this.getEasyNPCUUID(), this.factionSelectBox.getSelectedValue());
                  }
                }));
    this.factionEditButton.active = this.hasFactionSelected();
    this.factionEditButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("edit_faction")));

    this.newFactionButton =
        this.addRenderableWidget(
            new AddButton(
                this.factionEditButton.getX() + this.factionEditButton.getWidth() + 2,
                factionEntriesTop,
                60,
                "new",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openFactionsEditor(this.getEasyNPCUUID())));
    this.newFactionButton.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("new_faction.tooltip")));

    this.addRenderableWidget(
        new HelpIcon(
            this.contentLeftPos
                + 10
                + this.font.width(TextComponent.getTranslatedConfigText("faction_name"))
                + 4,
            factionEntriesTop + 3,
            "faction_name.tooltip"));

    this.initShoulderSection();
  }

  @Override
  protected void renderBg(
      GuiGraphicsExtractor guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    if (this.factionSelectBox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "faction_name",
          this.contentLeftPos + 10,
          this.factionSelectBox.getY() + 4);
    }

    if (this.sitOnShoulderCheckbox == null) {
      return;
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "shoulder_settings",
        this.contentLeftPos + 10,
        this.shoulderSectionTop,
        0x555555);
    for (InteractionAttributeType attributeType : SHOULDER_OFFSET_ATTRIBUTES) {
      EditBox shoulderOffsetBox = this.shoulderOffsetBoxes.get(attributeType);
      Text.drawConfigString(
          guiGraphics,
          this.font,
          attributeType.getAttributeName(),
          this.contentLeftPos + 10,
          shoulderOffsetBox.getY() + 4);
    }
  }
}
