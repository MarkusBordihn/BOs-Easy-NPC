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

package de.markusbordihn.easynpc.configui.client.screen.editor.faction;

import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.AddButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.ColorButton;
import de.markusbordihn.easynpc.configui.client.screen.components.ColorPickerPopup;
import de.markusbordihn.easynpc.configui.client.screen.components.DeleteButton;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectBox;
import de.markusbordihn.easynpc.configui.client.screen.components.SelectOption;
import de.markusbordihn.easynpc.configui.client.screen.components.UpDownButton;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.data.FactionDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class FactionEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  private static final int MAX_VISIBLE_HOSTILE_FACTIONS = 7;
  private static final int ROW_HEIGHT = 16;
  private static final int SECTION_COLOR = 0xFF808080;
  private final List<String> hostileFactionNames = new ArrayList<>();
  protected Button homeButton;
  protected Button factionsBreadcrumbButton;
  protected ColorButton factionColorButton;
  protected ColorPickerPopup colorPickerPopup;
  protected SelectBox<String> addHostileFactionSelectBox;
  protected Checkbox mutualHostileFactionCheckbox;
  protected Button addHostileFactionButton;
  protected Button deleteFactionButton;
  protected UpDownButton hostileFactionsScrollButton;
  private Map<String, FactionDataEntry> factionDataEntries;
  private FactionDataEntry factionDataEntry;
  private String factionName = "";
  private int hostileFactionsTop;
  private int hostileFactionsScrollOffset;

  public FactionEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static int[] getChatFormattingPalette() {
    List<Integer> colors = new ArrayList<>();
    for (ChatFormatting chatFormatting : ChatFormatting.values()) {
      if (chatFormatting.isColor() && chatFormatting.getColor() != null) {
        colors.add(chatFormatting.getColor());
      }
    }
    return colors.stream().mapToInt(Integer::intValue).toArray();
  }

  private static ChatFormatting getChatFormattingByColor(int color) {
    for (ChatFormatting chatFormatting : ChatFormatting.values()) {
      if (chatFormatting.isColor()
          && chatFormatting.getColor() != null
          && chatFormatting.getColor() == color) {
        return chatFormatting;
      }
    }
    return null;
  }

  private void openFactionsOverview() {
    NetworkMessageHandlerManager.getServerHandler().openFactionsEditor(this.getEasyNPCUUID());
  }

  private void deleteFaction() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(
        new ConfirmScreen(
            confirmed -> {
              if (confirmed && this.getEasyNPCUUID() != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .removeFactionEntry(this.getEasyNPCUUID(), this.factionName);
              } else {
                minecraft.setScreen(this);
              }
            },
            TextComponent.getTranslatedConfigText("removeFaction.deleteQuestion"),
            TextComponent.getTranslatedConfigText("removeFaction.deleteWarning", this.factionName),
            TextComponent.getTranslatedConfigText("removeFaction.deleteButton"),
            CommonComponents.GUI_CANCEL));
  }

  private List<String> getVisibleHostileFactionNames() {
    int fromIndex = Math.min(this.hostileFactionsScrollOffset, this.hostileFactionNames.size());
    int toIndex =
        Math.min(fromIndex + MAX_VISIBLE_HOSTILE_FACTIONS, this.hostileFactionNames.size());
    return this.hostileFactionNames.subList(fromIndex, toIndex);
  }

  private int getMaxScrollOffset() {
    return Math.max(0, this.hostileFactionNames.size() - MAX_VISIBLE_HOSTILE_FACTIONS);
  }

  private void scrollHostileFactions(int direction) {
    int newScrollOffset =
        Math.max(
            0, Math.min(this.hostileFactionsScrollOffset + direction, this.getMaxScrollOffset()));
    if (newScrollOffset != this.hostileFactionsScrollOffset) {
      this.hostileFactionsScrollOffset = newScrollOffset;
      this.rebuildWidgets();
    }
  }

  @Override
  public void init() {
    super.init();

    this.factionDataEntries =
        AdditionalScreenData.getFactionData(this.getAdditionalScreenData().getData());
    this.factionName =
        this.getAdditionalScreenData()
            .getData()
            .getString(FactionDataCapable.DATA_FACTION_NAME_TAG)
            .orElse("");
    this.factionDataEntry =
        this.factionDataEntries.getOrDefault(
            this.factionName, new FactionDataEntry(this.factionName));

    this.hostileFactionNames.clear();
    this.hostileFactionNames.addAll(new TreeSet<>(this.factionDataEntry.getHostileFactions()));
    this.hostileFactionsScrollOffset =
        Math.min(this.hostileFactionsScrollOffset, this.getMaxScrollOffset());

    // Breadcrumb navigation: < Factions > current faction.
    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 3,
                this.topPos + 3,
                10,
                16,
                "<",
                onPress -> this.openFactionsOverview()));
    this.factionsBreadcrumbButton =
        this.addRenderableWidget(
            new TextButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.homeButton.getY(),
                70,
                TextComponent.getTranslatedConfigText("factions_editor"),
                onPress -> this.openFactionsOverview()));
    Button factionNameButton =
        this.addRenderableWidget(
            new TextButton(
                this.factionsBreadcrumbButton.getX() + this.factionsBreadcrumbButton.getWidth(),
                this.homeButton.getY(),
                110,
                Component.literal("> ")
                    .append(FactionsEditorScreen.getFactionNameComponent(this.factionDataEntry)),
                onPress -> {}));
    factionNameButton.active = false;

    // Faction color with the shared color picker popup, restricted to the 16 team colors.
    int currentColor =
        this.factionDataEntry.getColor() != null
                && this.factionDataEntry.getColor().getColor() != null
            ? this.factionDataEntry.getColor().getColor()
            : 0xFFFFFF;
    this.colorPickerPopup =
        new ColorPickerPopup(
            this.font,
            getChatFormattingPalette(),
            8,
            false,
            selectedColor -> {
              ChatFormatting chatFormatting = getChatFormattingByColor(selectedColor);
              if (chatFormatting != null) {
                this.factionColorButton.setColorValue(selectedColor);
                NetworkMessageHandlerManager.getServerHandler()
                    .changeFactionColor(this.getEasyNPCUUID(), this.factionName, chatFormatting);
              }
            });
    this.factionColorButton =
        this.addRenderableWidget(
            new ColorButton(
                this.leftPos + 100,
                this.topPos + 24,
                onPress ->
                    this.colorPickerPopup.toggle(
                        this.factionColorButton.getColorValue(),
                        this.factionColorButton.getX(),
                        this.factionColorButton.getY() + this.factionColorButton.getHeight() + 1,
                        this.width,
                        this.height)));
    this.factionColorButton.setColorValue(currentColor);
    this.addRenderableWidget(
        new HelpIcon(
            this.leftPos
                + 10
                + this.font.width(TextComponent.getTranslatedConfigText("faction_color"))
                + 4,
            this.topPos + 27,
            "faction_color.tooltip"));

    // Scrollable list of hostile factions with a delete button column.
    this.hostileFactionsTop = this.topPos + 76;
    this.addRenderableWidget(
        new HelpIcon(
            this.getSectionTitleEnd("attacks_factions"),
            this.topPos + 44,
            "attacks_factions.tooltip"));
    int hostileFactionTop = this.hostileFactionsTop;
    for (String hostileFactionName : this.getVisibleHostileFactionNames()) {
      this.addRenderableWidget(
          new DeleteButton(
              this.leftPos + 272,
              hostileFactionTop - 2,
              onPress ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .changeFactionRelation(
                          this.getEasyNPCUUID(),
                          this.factionName,
                          hostileFactionName,
                          false,
                          false)));
      hostileFactionTop += ROW_HEIGHT;
    }
    if (this.hostileFactionNames.size() > MAX_VISIBLE_HOSTILE_FACTIONS) {
      this.hostileFactionsScrollButton =
          this.addRenderableWidget(
              new UpDownButton(
                  this.leftPos + 300,
                  this.hostileFactionsTop,
                  14,
                  36,
                  upDownButton -> this.scrollHostileFactions(-1),
                  upDownButton -> this.scrollHostileFactions(1)));
    }

    // Add new hostile faction based on the known factions, with optional mutual relation.
    List<SelectOption<String>> hostileFactionCandidates = new ArrayList<>();
    for (String availableFactionName : this.factionDataEntries.keySet()) {
      if (!availableFactionName.equals(this.factionName)
          && !this.factionDataEntry.isHostileTo(availableFactionName)) {
        hostileFactionCandidates.add(SelectOption.of(availableFactionName));
      }
    }
    int addHostileFactionTop = this.topPos + 190;
    this.addHostileFactionSelectBox =
        this.addRenderableWidget(
            new SelectBox<>(
                this.leftPos + 8,
                addHostileFactionTop,
                110,
                16,
                hostileFactionCandidates,
                selectedFactionName ->
                    this.addHostileFactionButton.active = selectedFactionName != null));
    this.addHostileFactionSelectBox.active = !hostileFactionCandidates.isEmpty();
    this.addHostileFactionButton =
        this.addRenderableWidget(
            new AddButton(
                this.leftPos + 122,
                addHostileFactionTop,
                60,
                "add",
                onPress -> {
                  String selectedFactionName = this.addHostileFactionSelectBox.getSelectedValue();
                  if (selectedFactionName != null) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeFactionRelation(
                            this.getEasyNPCUUID(),
                            this.factionName,
                            selectedFactionName,
                            true,
                            this.mutualHostileFactionCheckbox.selected());
                  }
                }));
    this.addHostileFactionButton.active = false;
    this.mutualHostileFactionCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.leftPos + 190, addHostileFactionTop + 1, "mutual", false, checkbox -> {}));
    this.addRenderableWidget(
        new HelpIcon(this.leftPos + 250, addHostileFactionTop + 2, "mutual.tooltip"));

    // Delete the whole faction.
    this.deleteFactionButton =
        this.addRenderableWidget(
            new DeleteButton(
                this.leftPos + (328 - 140) / 2,
                this.bottomPos - 28,
                140,
                "removeFaction.deleteButton",
                onPress -> this.deleteFaction()));
  }

  private int getSectionTitleEnd(String translationKey) {
    return this.leftPos
        + 164
        + this.font.width(TextComponent.getTranslatedConfigText(translationKey)) / 2
        + 4;
  }

  private void renderSection(
      GuiGraphicsExtractor guiGraphics,
      String translationKey,
      String descriptionKey,
      int titleTop) {
    Component titleComponent = TextComponent.getTranslatedConfigText(translationKey);
    Text.drawString(
        guiGraphics,
        this.font,
        titleComponent,
        this.leftPos + 164 - this.font.width(titleComponent) / 2,
        titleTop,
        SECTION_COLOR);
    guiGraphics.fillGradient(
        this.leftPos + 8, titleTop + 10, this.leftPos + 320, titleTop + 11, 0x60808080, 0x60808080);
    Text.drawConfigString(
        guiGraphics, this.font, descriptionKey, this.leftPos + 10, titleTop + 14, SECTION_COLOR);
  }

  private Component getFactionRowComponent(String rowFactionName) {
    FactionDataEntry rowFactionDataEntry = this.factionDataEntries.get(rowFactionName);
    return rowFactionDataEntry != null
        ? FactionsEditorScreen.getFactionNameComponent(rowFactionDataEntry)
        : Component.literal(rowFactionName);
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    // While the color picker popup covers the pointer, report the pointer off-screen to the
    // underlying screen so the widgets below the popup do not render a hover highlight.
    if (this.colorPickerPopup != null && this.colorPickerPopup.isMouseOver(x, y)) {
      super.extractRenderState(guiGraphics, -1, -1, partialTicks);
    } else {
      super.extractRenderState(guiGraphics, x, y, partialTicks);
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "faction_color",
        this.leftPos + 10,
        this.topPos + 29,
        Constants.FONT_COLOR_BLACK);

    this.renderSection(
        guiGraphics, "attacks_factions", "attacks_factions.description", this.topPos + 46);
    int hostileFactionTop = this.hostileFactionsTop;
    if (this.hostileFactionNames.isEmpty()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "no_factions_listed",
          this.leftPos + 14,
          hostileFactionTop + 2,
          0xFF888888);
    }
    for (String hostileFactionName : this.getVisibleHostileFactionNames()) {
      Text.drawString(
          guiGraphics,
          this.font,
          this.getFactionRowComponent(hostileFactionName),
          this.leftPos + 14,
          hostileFactionTop + 2);
      hostileFactionTop += ROW_HEIGHT;
    }

    if (this.colorPickerPopup != null) {
      this.colorPickerPopup.render(guiGraphics, x, y, partialTicks);
    }
  }

  @Override
  public boolean mouseScrolled(double mouseX, double mouseY, double scrollX, double scrollY) {
    if (mouseX >= this.leftPos + 8
        && mouseX <= this.leftPos + 320
        && mouseY >= this.hostileFactionsTop
        && mouseY <= this.hostileFactionsTop + MAX_VISIBLE_HOSTILE_FACTIONS * ROW_HEIGHT) {
      this.scrollHostileFactions(scrollY > 0 ? -1 : 1);
      return true;
    }
    return super.mouseScrolled(mouseX, mouseY, scrollX, scrollY);
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.mouseClicked(mouseButtonEvent, doubleClick)) {
      return true;
    }
    return super.mouseClicked(mouseButtonEvent, doubleClick);
  }

  @Override
  public boolean keyPressed(KeyEvent keyEvent) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.isVisible()
        && this.colorPickerPopup.keyPressed(keyEvent)) {
      return true;
    }
    return super.keyPressed(keyEvent);
  }

  @Override
  public boolean charTyped(CharacterEvent characterEvent) {
    if (this.colorPickerPopup != null
        && this.colorPickerPopup.isVisible()
        && this.colorPickerPopup.charTyped(characterEvent)) {
      return true;
    }
    return super.charTyped(characterEvent);
  }
}
