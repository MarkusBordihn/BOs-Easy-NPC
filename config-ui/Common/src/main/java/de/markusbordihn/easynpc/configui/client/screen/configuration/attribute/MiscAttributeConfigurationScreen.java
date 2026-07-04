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
import de.markusbordihn.easynpc.configui.client.screen.components.AddButton;
import de.markusbordihn.easynpc.configui.client.screen.components.EditButton;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectBox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class MiscAttributeConfigurationScreen<T extends ConfigurationMenu>
    extends AttributeConfigurationScreen<T> {

  protected SelectBox<String> factionSelectBox;
  protected Button factionEditButton;
  protected Button newFactionButton;

  public MiscAttributeConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private boolean hasFactionSelected() {
    return this.factionSelectBox != null
        && this.factionSelectBox.getSelectedValue() != null
        && !this.factionSelectBox.getSelectedValue().isEmpty();
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.miscAttributeButton.active = false;

    int factionEntriesTop = this.contentTopPos + 20;
    int factionEntriesSecondColumn = this.contentLeftPos + 100;

    // NPC faction selection based on the known factions.
    String savedFactionName =
        this.getAdditionalScreenData()
            .getData()
            .getString(FactionDataCapable.DATA_FACTION_NAME_TAG);
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
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    if (this.factionSelectBox != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "faction_name",
          this.contentLeftPos + 10,
          this.factionSelectBox.getY() + 4);
    }
  }
}
