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

package de.markusbordihn.easynpc.client.screen.dialog;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.renderer.screen.EntityScreenRenderer;
import de.markusbordihn.easynpc.client.screen.Screen;
import de.markusbordihn.easynpc.client.screen.components.DialogBackwardButton;
import de.markusbordihn.easynpc.client.screen.components.DialogForwardButton;
import de.markusbordihn.easynpc.client.screen.components.DialogTextButton;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.compat.IntegrationRegistry;
import de.markusbordihn.easynpc.condition.ClientConditionEvaluator;
import de.markusbordihn.easynpc.config.ClientDialogConfig;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonConditionMode;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogMetaData;
import de.markusbordihn.easynpc.data.dialog.DialogOptionsData;
import de.markusbordihn.easynpc.data.dialog.DialogScreenLayout;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import de.markusbordihn.easynpc.data.screen.AdditionalScreenData;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.screens.inventory.tooltip.ClientTooltipComponent;
import net.minecraft.client.gui.screens.inventory.tooltip.DefaultTooltipPositioner;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class DialogScreen<T extends DialogMenu> extends Screen<T, AdditionalScreenData> {

  private static final int MAX_NUMBER_OF_DIALOG_LINES = 10;
  private static final int MAX_TOTAL_DIALOG_LINES = 100;
  private static DialogScreenLayout dialogScreenLayout = DialogScreenLayout.UNKNOWN;
  protected final ArrayList<Button> dialogButtons = new ArrayList<>();
  protected final ArrayList<DialogButtonEntry> dialogButtonEntries = new ArrayList<>();
  protected final Component dialogText;
  protected final DialogMetaData dialogMetaData;
  protected Button dialogForwardButton = null;
  protected Button dialogBackwardButton = null;
  protected String dialog;
  protected Component dialogComponent;
  protected int numberOfDialogLines = 1;
  protected int dialogPageIndex = 0;
  private List<FormattedCharSequence> cachedDialogComponents = Collections.emptyList();
  private int[] cachedLineLengths = new int[0];
  private boolean hasConditionalButtons = false;
  private DialogOptionsData cachedDialogOptions = DialogOptionsData.DEFAULT;
  private boolean typewriterEnabled;
  private int charsPerSecond;
  private long pageStartTimeMillis;
  private boolean pageFullyRevealed;

  public DialogScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component, 280, 200);
    this.dialogText = this.getDialogText();
    this.dialogMetaData =
        new DialogMetaData(
            this.getEasyNPC().getLivingEntity(),
            minecraftInstance != null ? minecraftInstance.player : null,
            this.getAdditionalScreenData() != null
                ? this.getAdditionalScreenData().getScoreboardData()
                : null);
  }

  private static void setDialogScreenLayout(DialogScreenLayout dialogScreenLayout) {
    DialogScreen.dialogScreenLayout = dialogScreenLayout;
  }

  protected void renderDialog(GuiGraphics guiGraphics) {
    int dialogTopPosition = this.topPos + 20;

    if (this.cachedDialogComponents.isEmpty()) {
      return;
    }

    int pageStart = this.dialogPageIndex * MAX_NUMBER_OF_DIALOG_LINES;
    int pageEnd = Math.min(this.numberOfDialogLines, pageStart + MAX_NUMBER_OF_DIALOG_LINES);
    int revealed = this.revealedChars();
    int consumed = 0;

    for (int line = pageStart; line < pageEnd; line++) {
      int textTopPosition = dialogTopPosition + 6 + (line - pageStart) * (font.lineHeight + 2);
      int lineLength = this.cachedLineLengths[line];
      int show = Math.min(lineLength, Math.max(0, revealed - consumed));
      FormattedCharSequence formattedCharSequence =
          show >= lineLength
              ? this.cachedDialogComponents.get(line)
              : Text.limit(this.cachedDialogComponents.get(line), show);
      Text.drawString(
          guiGraphics, this.font, formattedCharSequence, this.leftPos + 87, textTopPosition, 0);
      consumed += lineLength;
    }
  }

  private void beginPageReveal() {
    this.pageStartTimeMillis = System.currentTimeMillis();
    this.pageFullyRevealed = false;
  }

  private int revealedChars() {
    if (!this.typewriterEnabled || this.pageFullyRevealed) {
      return Integer.MAX_VALUE;
    }
    long elapsed = System.currentTimeMillis() - this.pageStartTimeMillis;
    return (int) (elapsed * this.charsPerSecond / 1000L);
  }

  private int currentPageTotalChars() {
    int pageStart = this.dialogPageIndex * MAX_NUMBER_OF_DIALOG_LINES;
    int pageEnd = Math.min(this.numberOfDialogLines, pageStart + MAX_NUMBER_OF_DIALOG_LINES);
    pageEnd = Math.min(pageEnd, this.cachedLineLengths.length);
    int total = 0;
    for (int line = pageStart; line < pageEnd; line++) {
      total += this.cachedLineLengths[line];
    }
    return total;
  }

  private boolean shouldHideUnavailableDialogButtons() {
    return this.cachedDialogOptions.buttonConditionMode() == DialogButtonConditionMode.HIDE;
  }

  private boolean isTypewriterActive() {
    return this.typewriterEnabled
        && !this.pageFullyRevealed
        && this.revealedChars() < this.currentPageTotalChars();
  }

  private void setDialogText(DialogDataEntry dialogData) {
    if (dialogData == null) {
      return;
    }
    String dialogText = dialogData.getDialogText(this.dialogMetaData);
    if (dialogText == null || dialogText.isBlank()) {
      return;
    }

    this.dialogComponent = TextComponent.getText(dialogText);
    this.cachedDialogComponents =
        this.font.split(this.dialogComponent, DialogUtils.MAX_DIALOG_LINE_LENGTH);
    this.numberOfDialogLines = Math.min(MAX_TOTAL_DIALOG_LINES, this.cachedDialogComponents.size());
    this.cachedLineLengths = new int[this.cachedDialogComponents.size()];
    for (int line = 0; line < this.cachedDialogComponents.size(); line++) {
      this.cachedLineLengths[line] = Text.length(this.cachedDialogComponents.get(line));
    }
  }

  private void addDialogButton(DialogButtonEntry dialogButtonEntry) {
    if (dialogButtonEntry == null) {
      return;
    }

    Component fullButtonName =
        TextComponent.getTextComponentRaw(
            dialogButtonEntry.name(), dialogButtonEntry.isTranslationKey());

    DialogTextButton dialogButton =
        new DialogTextButton(
            this.leftPos + 70,
            this.topPos + 55,
            198,
            fullButtonName,
            dialogButtonEntry.hasConditions(),
            onPress -> {
              if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_BUTTON_CLICK)) {
                NetworkMessageHandlerManager.getServerHandler()
                    .executeActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_BUTTON_CLICK);
              }

              if (dialogButtonEntry.hasActionData()) {
                UUID buttonId = dialogButtonEntry.id();
                NetworkMessageHandlerManager.getServerHandler()
                    .executeDialogButtonAction(
                        this.getEasyNPCUUID(), this.getDialogUUID(), buttonId);
              } else {
                this.onClose();
              }
            });

    dialogButton.visible = dialogButtonEntry.name() != null && !dialogButtonEntry.name().isBlank();

    this.dialogButtons.add(dialogButton);
    this.dialogButtonEntries.add(dialogButtonEntry);
    if (dialogButtonEntry != null && dialogButtonEntry.hasConditions()) {
      this.hasConditionalButtons = true;
    }
    this.updateDialogButtonLockState(dialogButton, dialogButtonEntry);
    this.addRenderableWidget(dialogButton);
  }

  private boolean isDialogButtonUnavailable(DialogButtonEntry dialogButtonEntry) {
    if (dialogButtonEntry == null || !dialogButtonEntry.hasConditions()) {
      return false;
    }
    boolean conditionsMet =
        ClientConditionEvaluator.evaluateAll(
            dialogButtonEntry.conditions(),
            minecraftInstance != null ? minecraftInstance.player : null,
            this.getEasyNPC() != null ? this.getEasyNPC().getLivingEntity() : null);
    AdditionalScreenData additionalScreenData = this.getAdditionalScreenData();
    boolean executionLimitReached =
        additionalScreenData != null
            && additionalScreenData.isExecutionLimitReached(dialogButtonEntry.id());
    boolean dialogButtonLocked =
        additionalScreenData != null
            && additionalScreenData.isDialogButtonLocked(dialogButtonEntry.id());
    return !conditionsMet || executionLimitReached || dialogButtonLocked;
  }

  private void updateDialogButtonLockState(
      Button dialogButton, DialogButtonEntry dialogButtonEntry) {
    boolean unavailable = this.isDialogButtonUnavailable(dialogButtonEntry);
    dialogButton.active = !unavailable;
    dialogButton.visible =
        dialogButtonEntry.name() != null
            && !dialogButtonEntry.name().isBlank()
            && (!unavailable || !this.shouldHideUnavailableDialogButtons());
  }

  private List<Button> getVisibleDialogButtons() {
    List<Button> visibleDialogButtons = new ArrayList<>();
    for (Button dialogButton : this.dialogButtons) {
      if (dialogButton.visible) {
        visibleDialogButtons.add(dialogButton);
      }
    }
    return visibleDialogButtons;
  }

  private List<DialogButtonEntry> getVisibleDialogButtonEntries() {
    List<DialogButtonEntry> visibleDialogButtonEntries = new ArrayList<>();
    for (int i = 0; i < this.dialogButtons.size() && i < this.dialogButtonEntries.size(); i++) {
      if (this.dialogButtons.get(i).visible) {
        visibleDialogButtonEntries.add(this.dialogButtonEntries.get(i));
      }
    }
    return visibleDialogButtonEntries;
  }

  private void renderDialogButtons() {
    List<Button> visibleDialogButtons = this.getVisibleDialogButtons();
    if (!DialogButtonLayout.apply(
        dialogScreenLayout, visibleDialogButtons, this.leftPos, this.topPos)) {
      log.warn(
          "Unknown dialog screen layout {} for {} with {} line(s)",
          dialogScreenLayout,
          this.getDialogDataSet(),
          this.numberOfDialogLines);
    }
  }

  private void defineDialogNavigationButtons() {
    int dialogNavigationButtonTopPosition =
        dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_ONLY
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_ONE_BUTTON
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_TWO_BUTTONS
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_THREE_BUTTONS
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_FOUR_BUTTONS
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_FIVE_BUTTONS
                || dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_SIX_BUTTONS
            ? this.topPos + 97
            : this.topPos + 138;

    this.dialogForwardButton =
        this.addRenderableWidget(
            new DialogForwardButton(
                this.leftPos + 266,
                dialogNavigationButtonTopPosition,
                onPress -> {
                  this.dialogPageIndex =
                      this.dialogPageIndex < this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES
                          ? this.dialogPageIndex + 1
                          : 0;
                  this.beginPageReveal();
                  if (this.dialogBackwardButton != null) {
                    this.dialogBackwardButton.active = this.dialogPageIndex > 0;
                  }
                  if (this.dialogForwardButton != null) {
                    this.dialogForwardButton.active =
                        this.dialogPageIndex
                            < this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES;
                  }
                }));
    this.dialogForwardButton.active =
        this.dialogPageIndex < this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES;

    this.dialogBackwardButton =
        this.addRenderableWidget(
            new DialogBackwardButton(
                this.leftPos + 254,
                dialogNavigationButtonTopPosition,
                onPress -> {
                  this.dialogPageIndex =
                      this.dialogPageIndex > 0
                          ? this.dialogPageIndex - 1
                          : this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES;
                  this.beginPageReveal();
                  if (this.dialogForwardButton != null) {
                    this.dialogForwardButton.active =
                        this.dialogPageIndex
                            < this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES;
                  }
                  if (this.dialogBackwardButton != null) {
                    this.dialogBackwardButton.active = this.dialogPageIndex > 0;
                  }
                }));
    this.dialogBackwardButton.active = this.dialogPageIndex > 0;
  }

  @Override
  public void init() {
    if (this.hasDialogData()) {
      this.cachedDialogOptions = this.getDialogData().getDialogOptions();
      if (!this.cachedDialogOptions.showCloseButton()) {
        this.showCloseButton = false;
      }
    }

    super.init();

    this.topPos -= 4;
    this.bottomPos -= 4;

    this.dialogButtons.clear();
    this.dialogButtonEntries.clear();
    this.hasConditionalButtons = false;

    this.titleLabelX = 10;
    this.titleLabelY = 8;

    if (this.closeButton != null) {
      this.closeButton.setX(this.leftPos + this.imageWidth - 5);
      this.closeButton.setY(this.topPos + 4);
    }

    this.setDialogText(this.getDialogData());

    this.typewriterEnabled = ClientDialogConfig.TYPEWRITER_ENABLED;
    this.charsPerSecond = Math.max(1, ClientDialogConfig.TYPEWRITER_CHARS_PER_SECOND);
    this.beginPageReveal();

    if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_OPEN_DIALOG)) {
      NetworkMessageHandlerManager.getServerHandler()
          .executeActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_OPEN_DIALOG);
    }

    if (this.hasDialogData() && this.getDialogData().getNumberOfDialogButtons() > 0) {
      this.dialogButtons.ensureCapacity(this.getDialogData().getNumberOfDialogButtons());
      for (DialogButtonEntry dialogButtonEntry : this.getDialogData().getDialogButtons()) {
        if (dialogButtonEntry == null) {
          continue;
        }
        this.addDialogButton(dialogButtonEntry);
      }
    }

    setDialogScreenLayout(
        DialogUtils.getDialogScreenLayout(
            this.dialogComponent, this.font, this.getVisibleDialogButtonEntries()));
    log.debug(
        "Prepare Dialog Screen {} with page index {} for {} with {} line(s) and layout {}",
        this.getDialogUUID(),
        this.getPageIndex(),
        this.getDialogDataSet(),
        this.numberOfDialogLines,
        dialogScreenLayout);

    if (this.numberOfDialogLines > MAX_NUMBER_OF_DIALOG_LINES) {
      this.defineDialogNavigationButtons();
    }
    this.renderDialogButtons();
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    if (this.getEasyNPC() == null) {
      return;
    }
    super.render(guiGraphics, x, y, partialTicks);

    if (!this.cachedDialogOptions.displayAvatar()) {
      this.renderDialog(guiGraphics);
      this.renderDialogButtonLocks(guiGraphics, x, y);
      return;
    }

    int entityTop =
        this.cachedDialogOptions.hasAvatarTop()
            ? this.cachedDialogOptions.avatarTop()
            : this.getEasyNPC().getEasyNPCDialogData().getEntityDialogTop();
    int entityLeft =
        this.cachedDialogOptions.hasAvatarLeft()
            ? this.cachedDialogOptions.avatarLeft()
            : this.getEasyNPC().getEasyNPCDialogData().getEntityDialogLeft();
    int scale =
        this.cachedDialogOptions.hasAvatarScale()
            ? this.cachedDialogOptions.avatarScale()
            : this.getEasyNPC().getEasyNPCDialogData().getEntityDialogScaling();

    IntegrationRegistry.setGuiPreviewMode(true);
    EntityScreenRenderer.renderEntityRaw(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.dialog(
            this.leftPos + 40 + entityLeft, this.topPos + 80 + entityTop, scale),
        this.xMouse,
        this.yMouse);
    IntegrationRegistry.setGuiPreviewMode(false);

    this.renderDialog(guiGraphics);
    this.renderDialogButtonLocks(guiGraphics, x, y);
  }

  @Override
  protected int getUpdateTickInterval() {
    return 20;
  }

  @Override
  protected void updateTick() {
    super.updateTick();

    if (!this.hasConditionalButtons) {
      return;
    }
    for (int i = 0; i < this.dialogButtons.size() && i < this.dialogButtonEntries.size(); i++) {
      this.updateDialogButtonLockState(this.dialogButtons.get(i), this.dialogButtonEntries.get(i));
    }
    setDialogScreenLayout(
        DialogUtils.getDialogScreenLayout(
            this.dialogComponent, this.font, this.getVisibleDialogButtonEntries()));
    this.renderDialogButtons();
  }

  private void renderDialogButtonLocks(GuiGraphics guiGraphics, int mouseX, int mouseY) {
    Component tooltip = null;
    for (int i = 0; i < this.dialogButtons.size() && i < this.dialogButtonEntries.size(); i++) {
      Button dialogButton = this.dialogButtons.get(i);
      DialogButtonEntry dialogButtonEntry = this.dialogButtonEntries.get(i);
      if (!dialogButton.visible) {
        continue;
      }

      boolean locked =
          dialogButtonEntry != null && dialogButtonEntry.hasConditions() && !dialogButton.active;

      int lockLeft = dialogButton.getX() + dialogButton.getWidth() - 12;
      int lockTop = dialogButton.getY() + (dialogButton.getHeight() - 8) / 2;
      if (locked) {
        int lockColor = 0xffd0d0d0;
        guiGraphics.fill(lockLeft + 2, lockTop, lockLeft + 5, lockTop + 1, lockColor);
        guiGraphics.fill(lockLeft + 1, lockTop + 1, lockLeft + 2, lockTop + 3, lockColor);
        guiGraphics.fill(lockLeft + 5, lockTop + 1, lockLeft + 6, lockTop + 3, lockColor);
        guiGraphics.fill(lockLeft, lockTop + 3, lockLeft + 7, lockTop + 8, lockColor);
      }

      boolean overButton =
          mouseX >= dialogButton.getX()
              && mouseX < dialogButton.getX() + dialogButton.getWidth()
              && mouseY >= dialogButton.getY()
              && mouseY < dialogButton.getY() + dialogButton.getHeight();
      if (!overButton) {
        continue;
      }

      boolean overLock =
          locked
              && mouseX >= lockLeft
              && mouseX < lockLeft + 7
              && mouseY >= lockTop
              && mouseY < lockTop + 8;
      if (overLock) {
        tooltip = TextComponent.getTranslatedText("dialog.button.locked");
      } else if (dialogButton instanceof DialogTextButton dialogTextButton
          && dialogTextButton.isTextTruncated()) {
        tooltip = dialogButton.getMessage();
      }
    }

    if (tooltip != null) {
      guiGraphics.renderTooltip(
          this.font,
          Collections.singletonList(ClientTooltipComponent.create(tooltip.getVisualOrderText())),
          mouseX,
          mouseY,
          DefaultTooltipPositioner.INSTANCE,
          null);
    }
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int x, int y) {
    Text.drawString(
        guiGraphics,
        this.font,
        this.title,
        this.leftPos + this.titleLabelX,
        this.topPos + this.titleLabelY);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    switch (dialogScreenLayout) {
      case UNKNOWN:
        break;
      case COMPACT_TEXT_ONLY,
      COMPACT_TEXT_WITH_ONE_BUTTON,
      COMPACT_TEXT_WITH_TWO_BUTTONS,
      COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        Graphics.blit(
            guiGraphics,
            Constants.TEXTURE_DIALOG_SCENE_SMALL,
            this.leftPos,
            this.topPos,
            1,
            1,
            295,
            176,
            512,
            256);
        break;
      case COMPACT_TEXT_WITH_THREE_BUTTONS,
      COMPACT_TEXT_WITH_FOUR_BUTTONS,
      COMPACT_TEXT_WITH_FIVE_BUTTONS,
      COMPACT_TEXT_WITH_SIX_BUTTONS:
        Graphics.blit(
            guiGraphics,
            Constants.TEXTURE_DIALOG_SCENE_MEDIUM,
            this.leftPos,
            this.topPos,
            1,
            1,
            295,
            216,
            512,
            256);
        break;
      default:
        Graphics.blit(
            guiGraphics,
            Constants.TEXTURE_DIALOG_SCENE_LARGE,
            this.leftPos,
            this.topPos,
            1,
            1,
            295,
            216,
            512,
            256);
    }
  }

  @Override
  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (this.isTypewriterActive()) {
      this.pageFullyRevealed = true;
    }
    return super.mouseClicked(mouseButtonEvent, doubleClick);
  }

  @Override
  public boolean keyPressed(KeyEvent keyEvent) {
    if (keyEvent.input() != 256 && this.isTypewriterActive()) {
      this.pageFullyRevealed = true;
    }

    if (keyEvent.input() == 256 && !this.cachedDialogOptions.allowEscClose()) {
      return true;
    }

    return super.keyPressed(keyEvent);
  }

  @Override
  public void onClose() {
    if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_CLOSE_DIALOG)) {
      NetworkMessageHandlerManager.getServerHandler()
          .executeActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_CLOSE_DIALOG);
    }
    super.onClose();
  }
}
