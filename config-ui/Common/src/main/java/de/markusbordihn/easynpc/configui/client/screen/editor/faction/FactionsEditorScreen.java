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
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.client.screen.EditorScreen;
import de.markusbordihn.easynpc.configui.client.screen.components.AddButton;
import de.markusbordihn.easynpc.configui.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.faction.FactionDataEntry;
import de.markusbordihn.easynpc.data.faction.FactionNameValidator;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.world.entity.player.Inventory;

public class FactionsEditorScreen<T extends EditorMenu> extends EditorScreen<T> {

  private static final int MAX_FACTIONS_PER_PAGE = 8;
  private static final int ROW_HEIGHT = 18;
  private static final int NAME_COLUMN_LEFT = 8;
  private static final int NAME_COLUMN_WIDTH = 92;
  private static final int ATTACKS_COLUMN_LEFT = 106;
  private static final int ATTACKS_COLUMN_WIDTH = 100;
  private static final int ATTACKED_BY_COLUMN_LEFT = 212;
  private static final int ATTACKED_BY_COLUMN_WIDTH = 104;
  private static final int SECTION_COLOR = 0xFF808080;
  private final List<FactionRow> factionRows = new ArrayList<>();
  protected Button homeButton;
  protected Button previousPageButton;
  protected Button nextPageButton;
  protected TextField newFactionNameTextField;
  protected Button addFactionButton;
  private Map<String, FactionDataEntry> factionDataEntries;
  private int factionRowsTop;
  private int pageOffset = 0;

  public FactionsEditorScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  static MutableComponent getFactionNameComponent(FactionDataEntry factionDataEntry) {
    MutableComponent factionNameComponent = Component.literal(factionDataEntry.getName());
    if (factionDataEntry.getColor() != null) {
      factionNameComponent.withStyle(
          style -> style.withColor(factionDataEntry.getColor().textColor()));
    }
    return factionNameComponent;
  }

  private void openPreviousScreen() {
    NetworkMessageHandlerManager.getServerHandler()
        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.MISC_ATTRIBUTE);
  }

  private List<FactionRow> getVisibleFactionRows() {
    int fromIndex = Math.min(this.pageOffset * MAX_FACTIONS_PER_PAGE, this.factionRows.size());
    int toIndex = Math.min(fromIndex + MAX_FACTIONS_PER_PAGE, this.factionRows.size());
    return this.factionRows.subList(fromIndex, toIndex);
  }

  @Override
  public void init() {
    super.init();

    this.factionDataEntries =
        AdditionalScreenData.getFactionData(this.getAdditionalScreenData().getData());

    this.factionRows.clear();
    for (FactionDataEntry factionDataEntry : this.factionDataEntries.values()) {
      List<String> attackedBy = new ArrayList<>();
      for (FactionDataEntry otherFactionDataEntry : this.factionDataEntries.values()) {
        if (otherFactionDataEntry.isHostileTo(factionDataEntry.getName())) {
          attackedBy.add(otherFactionDataEntry.getName());
        }
      }
      this.factionRows.add(
          new FactionRow(
              factionDataEntry.getName(),
              String.join(", ", new TreeSet<>(factionDataEntry.getHostileFactions())),
              String.join(", ", new TreeSet<>(attackedBy))));
    }

    this.homeButton =
        this.addRenderableWidget(
            new TextButton(
                this.leftPos + 3,
                this.topPos + 3,
                10,
                16,
                "<",
                onPress -> this.openPreviousScreen()));

    Button titleButton =
        this.addRenderableWidget(
            new TextButton(
                this.homeButton.getX() + this.homeButton.getWidth(),
                this.homeButton.getY(),
                140,
                TextComponent.getTranslatedConfigText("factions_editor"),
                onPress -> {}));
    titleButton.active = false;

    this.factionRowsTop = this.topPos + 44;
    int maxPageOffset = Math.max(0, (this.factionRows.size() - 1) / MAX_FACTIONS_PER_PAGE);
    this.pageOffset = Math.min(this.pageOffset, maxPageOffset);
    int factionRowTop = this.factionRowsTop;
    for (FactionRow factionRow : this.getVisibleFactionRows()) {
      FactionDataEntry rowFactionDataEntry = this.factionDataEntries.get(factionRow.name());
      this.addRenderableWidget(
          new TextButton(
              this.leftPos + NAME_COLUMN_LEFT,
              factionRowTop,
              NAME_COLUMN_WIDTH,
              rowFactionDataEntry != null
                  ? getFactionNameComponent(rowFactionDataEntry)
                  : Component.literal(factionRow.name()),
              onPress ->
                  NetworkMessageHandlerManager.getServerHandler()
                      .openFactionEditor(this.getEasyNPCUUID(), factionRow.name())));
      factionRowTop += ROW_HEIGHT;
    }

    if (maxPageOffset > 0) {
      this.previousPageButton =
          this.addRenderableWidget(
              new TextButton(
                  this.leftPos + 284,
                  this.bottomPos - 27,
                  16,
                  16,
                  "<",
                  onPress -> {
                    this.pageOffset = Math.max(0, this.pageOffset - 1);
                    this.rebuildWidgets();
                  }));
      this.previousPageButton.active = this.pageOffset > 0;
      this.nextPageButton =
          this.addRenderableWidget(
              new TextButton(
                  this.leftPos + 302,
                  this.bottomPos - 27,
                  16,
                  16,
                  ">",
                  onPress -> {
                    this.pageOffset++;
                    this.rebuildWidgets();
                  }));
      this.nextPageButton.active = this.pageOffset < maxPageOffset;
    }

    this.newFactionNameTextField =
        this.addRenderableWidget(
            new TextField(this.font, this.leftPos + 8, this.bottomPos - 27, 150));
    this.newFactionNameTextField.setFilter(FactionNameValidator::isValidInput);
    this.addFactionButton =
        this.addRenderableWidget(
            new AddButton(
                this.newFactionNameTextField.getX() + this.newFactionNameTextField.getWidth() + 5,
                this.bottomPos - 27,
                110,
                "add_faction",
                onPress -> {
                  String newFactionName = this.newFactionNameTextField.getValue();
                  if (FactionNameValidator.isValid(newFactionName)
                      && !this.factionDataEntries.containsKey(newFactionName)) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .createFaction(this.getEasyNPCUUID(), newFactionName);
                  }
                }));
    this.addFactionButton.active = false;
    this.newFactionNameTextField.setResponder(
        value ->
            this.addFactionButton.active =
                FactionNameValidator.isValid(value) && !this.factionDataEntries.containsKey(value));
  }

  private void renderColumn(
      GuiGraphicsExtractor guiGraphics,
      int mouseX,
      int mouseY,
      String columnText,
      int columnLeft,
      int columnWidth,
      int rowTop) {
    if (columnText.isEmpty()) {
      return;
    }

    int textLeft = this.leftPos + columnLeft;
    boolean truncated = this.font.width(columnText) > columnWidth;
    String visibleText =
        truncated
            ? this.font.plainSubstrByWidth(columnText, columnWidth - this.font.width("...")) + "..."
            : columnText;
    Text.drawString(guiGraphics, this.font, visibleText, textLeft, rowTop + 5, 0xFF404040);

    if (truncated
        && mouseX >= textLeft
        && mouseX < textLeft + columnWidth
        && mouseY >= rowTop
        && mouseY < rowTop + ROW_HEIGHT) {
      guiGraphics.setTooltipForNextFrame(this.font, Component.literal(columnText), mouseX, mouseY);
    }
  }

  @Override
  public void extractRenderState(
      GuiGraphicsExtractor guiGraphics, int x, int y, float partialTicks) {
    super.extractRenderState(guiGraphics, x, y, partialTicks);

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "faction_name",
        this.leftPos + NAME_COLUMN_LEFT + 2,
        this.topPos + 28,
        SECTION_COLOR);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "attacks_factions",
        this.leftPos + ATTACKS_COLUMN_LEFT,
        this.topPos + 28,
        SECTION_COLOR);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "attacked_by_factions",
        this.leftPos + ATTACKED_BY_COLUMN_LEFT,
        this.topPos + 28,
        SECTION_COLOR);
    guiGraphics.fillGradient(
        this.leftPos + 8,
        this.topPos + 38,
        this.leftPos + 320,
        this.topPos + 39,
        0x60808080,
        0x60808080);

    if (this.factionRows.isEmpty()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "no_factions_listed",
          this.leftPos + 14,
          this.factionRowsTop + 5,
          0xFF888888);
    }

    int factionRowTop = this.factionRowsTop;
    for (FactionRow factionRow : this.getVisibleFactionRows()) {
      this.renderColumn(
          guiGraphics,
          x,
          y,
          factionRow.attacks(),
          ATTACKS_COLUMN_LEFT,
          ATTACKS_COLUMN_WIDTH,
          factionRowTop);
      this.renderColumn(
          guiGraphics,
          x,
          y,
          factionRow.attackedBy(),
          ATTACKED_BY_COLUMN_LEFT,
          ATTACKED_BY_COLUMN_WIDTH,
          factionRowTop);
      factionRowTop += ROW_HEIGHT;
    }
  }

  private record FactionRow(String name, String attacks, String attackedBy) {}
}
