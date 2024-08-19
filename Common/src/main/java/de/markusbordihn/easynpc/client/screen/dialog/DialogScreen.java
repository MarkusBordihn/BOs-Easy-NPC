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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.Screen;
import de.markusbordihn.easynpc.client.screen.components.Graphics;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogMetaData;
import de.markusbordihn.easynpc.data.dialog.DialogScreenLayout;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class DialogScreen<T extends DialogMenu> extends Screen<T> {

  private static final int BUTTON_WIDTH = 126;
  private static final int MIDDLE_BUTTON_WIDTH = 200;
  private static final int LARGE_BUTTON_WIDTH = 250;
  private static final int MAX_NUMBER_OF_PIXEL_PER_LINE = 180;
  private static final int MAX_NUMBER_OF_DIALOG_LINES = 10;
  private static DialogScreenLayout dialogScreenLayout = DialogScreenLayout.UNKNOWN;
  protected final ArrayList<Button> dialogButtons = new ArrayList<>();
  protected final Component dialogText;
  protected final DialogMetaData dialogMetaData;
  protected Button dialogForwardButton = null;
  protected Button dialogBackwardButton = null;
  protected String dialog;
  protected Component dialogComponent;
  protected int numberOfDialogLines = 1;
  protected int dialogPageIndex = 0;
  private List<FormattedCharSequence> cachedDialogComponents = Collections.emptyList();

  public DialogScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.dialogText = this.getDialogText();
    this.dialogMetaData =
        new DialogMetaData(
            this.getEasyNPC().getLivingEntity(),
            minecraftInstance != null ? minecraftInstance.player : null);
  }

  private static void setDialogScreenLayout(DialogScreenLayout dialogScreenLayout) {
    DialogScreen.dialogScreenLayout = dialogScreenLayout;
  }

  protected void renderDialog(PoseStack poseStack) {
    // Draw dialog background bobble.
    int dialogTopPosition = topPos + 20;
    switch (dialogScreenLayout) {
      case COMPACT_TEXT_ONLY,
          COMPACT_TEXT_WITH_ONE_BUTTON,
          COMPACT_TEXT_WITH_TWO_BUTTONS,
          COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS,
          COMPACT_TEXT_WITH_THREE_BUTTONS,
          COMPACT_TEXT_WITH_FOUR_BUTTONS,
          COMPACT_TEXT_WITH_FIVE_BUTTONS,
          COMPACT_TEXT_WITH_SIX_BUTTONS:
        Graphics.blit(
            poseStack, Constants.TEXTURE_DIALOG, leftPos + 70, dialogTopPosition, 0, 120, 205, 78);
        break;
      default:
        Graphics.blit(
            poseStack, Constants.TEXTURE_DIALOG, leftPos + 70, dialogTopPosition, 0, 0, 205, 118);
    }

    // Distribute text for the across the lines and the give dialogPageIndex.
    if (!this.cachedDialogComponents.isEmpty()) {
      for (int line = this.dialogPageIndex * MAX_NUMBER_OF_DIALOG_LINES;
          line < this.numberOfDialogLines
              && line < MAX_NUMBER_OF_DIALOG_LINES * (this.dialogPageIndex + 1);
          ++line) {
        int textTopPosition =
            dialogTopPosition
                + 6
                + (line - (this.dialogPageIndex * MAX_NUMBER_OF_DIALOG_LINES))
                    * (font.lineHeight + 2);
        FormattedCharSequence formattedCharSequence = this.cachedDialogComponents.get(line);
        Text.drawString(
            poseStack, this.font, formattedCharSequence, leftPos + 87, textTopPosition, 0);
      }
    }
  }

  private void setDialogText(DialogDataEntry dialogData) {
    if (dialogData == null) {
      return;
    }
    String dialogText = dialogData.getDialogText(this.dialogMetaData);
    if (dialogText == null || dialogText.isBlank()) {
      return;
    }

    // Create dialog text component.
    this.dialogComponent = Component.literal(dialogText);

    // Split dialog text to lines.
    this.cachedDialogComponents =
        this.font.split(this.dialogComponent, MAX_NUMBER_OF_PIXEL_PER_LINE);
    this.numberOfDialogLines = Math.min(128 / font.lineHeight, this.cachedDialogComponents.size());
  }

  private void addDialogButton(DialogButtonEntry dialogButtonEntry) {
    if (dialogButtonEntry == null) {
      return;
    }

    // Create dialog button text and limit the length based on the dialog screen layout.
    int dialogButtonMaxTextLength =
        switch (dialogScreenLayout) {
          case COMPACT_TEXT_WITH_ONE_BUTTON,
                  TEXT_WITH_ONE_BUTTON,
                  TEXT_WITH_TWO_BUTTONS,
                  COMPACT_TEXT_WITH_THREE_BUTTONS,
                  TEXT_WITH_THREE_BUTTONS ->
              41;
          case COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS -> 32;
          default -> 22;
        };
    Component dialogButtonText = dialogButtonEntry.getButtonName(dialogButtonMaxTextLength);

    // Create dialog button.
    TextButton dialogButton =
        new TextButton(
            this.leftPos + 70,
            this.topPos + 55,
            198,
            dialogButtonText,
            onPress -> {
              // Action Event on button click.
              if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_BUTTON_CLICK)) {
                NetworkMessageHandlerManager.getServerHandler()
                    .triggerActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_BUTTON_CLICK);
              }

              // Custom action on button click.
              if (dialogButtonEntry.hasActionData()) {
                UUID buttonId = dialogButtonEntry.id();
                NetworkMessageHandlerManager.getServerHandler()
                    .triggerDialogButtonAction(
                        this.getEasyNPCUUID(), this.getDialogUUID(), buttonId);
              } else {
                this.closeScreen();
              }
            });

    // Set dialog button visibility.
    dialogButton.visible = dialogButtonEntry.name() != null && !dialogButtonEntry.name().isBlank();

    this.dialogButtons.add(dialogButton);
  }

  private Button renderDialogButton(int buttonIndex, int width, int left, int top) {
    Button dialogButton = this.dialogButtons.get(buttonIndex);
    dialogButton.setWidth(width);
    dialogButton.x = left;
    dialogButton.y = top;
    return this.addRenderableWidget(dialogButton);
  }

  private void renderDialogButtons() {
    // Render menu buttons specific on layout, if needed.
    switch (dialogScreenLayout) {
      case COMPACT_TEXT_ONLY, TEXT_ONLY:
        break;
      case COMPACT_TEXT_WITH_ONE_BUTTON:
        this.renderDialogButton(0, LARGE_BUTTON_WIDTH, this.leftPos + 18, this.topPos + 140);
        break;
      case COMPACT_TEXT_WITH_TWO_BUTTONS:
        Button firstCompactDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 140);
        this.renderDialogButton(
            1,
            BUTTON_WIDTH,
            firstCompactDialogButton.x + firstCompactDialogButton.getWidth() + 10,
            firstCompactDialogButton.y);
        break;
      case COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        Button firstCompactLargeDialogButton =
            this.renderDialogButton(0, MIDDLE_BUTTON_WIDTH, this.leftPos + 75, this.topPos + 115);
        this.renderDialogButton(
            1,
            MIDDLE_BUTTON_WIDTH,
            firstCompactLargeDialogButton.x,
            firstCompactLargeDialogButton.y + firstCompactLargeDialogButton.getHeight() + 10);
        break;
      case TEXT_WITH_ONE_BUTTON:
        this.renderDialogButton(0, LARGE_BUTTON_WIDTH, this.leftPos + 18, this.topPos + 170);
        break;
      case TEXT_WITH_TWO_BUTTONS:
        Button firstTwoDialogButton =
            this.renderDialogButton(0, LARGE_BUTTON_WIDTH, this.leftPos + 18, this.topPos + 145);
        this.renderDialogButton(
            1,
            LARGE_BUTTON_WIDTH,
            firstTwoDialogButton.x,
            firstTwoDialogButton.y + firstTwoDialogButton.getHeight() + 10);
        break;
      case COMPACT_TEXT_WITH_THREE_BUTTONS, TEXT_WITH_THREE_BUTTONS:
        Button firstThreeDialogButton =
            this.renderDialogButton(0, LARGE_BUTTON_WIDTH, this.leftPos + 18, this.topPos + 140);
        Button secondThreeDialogButton =
            this.renderDialogButton(
                1,
                LARGE_BUTTON_WIDTH,
                firstThreeDialogButton.x,
                firstThreeDialogButton.y + firstThreeDialogButton.getHeight() + 5);
        this.renderDialogButton(
            2,
            LARGE_BUTTON_WIDTH,
            secondThreeDialogButton.x,
            secondThreeDialogButton.y + secondThreeDialogButton.getHeight() + 5);
        break;
      case COMPACT_TEXT_WITH_FOUR_BUTTONS, TEXT_WITH_FOUR_BUTTONS:
        Button firstFourDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 150);
        Button secondFourDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstFourDialogButton.x + firstFourDialogButton.getWidth() + 10,
                firstFourDialogButton.y);
        Button thirdFourDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstFourDialogButton.x,
                firstFourDialogButton.y + firstFourDialogButton.getHeight() + 10);
        this.renderDialogButton(3, BUTTON_WIDTH, secondFourDialogButton.x, thirdFourDialogButton.y);
        break;
      case COMPACT_TEXT_WITH_FIVE_BUTTONS, TEXT_WITH_FIVE_BUTTONS:
        Button firstFiveDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 140);
        Button secondFiveDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstFiveDialogButton.x + firstFiveDialogButton.getWidth() + 10,
                firstFiveDialogButton.y);
        Button thirdFiveDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstFiveDialogButton.x,
                firstFiveDialogButton.y + firstFiveDialogButton.getHeight() + 5);
        this.renderDialogButton(3, BUTTON_WIDTH, secondFiveDialogButton.x, thirdFiveDialogButton.y);
        this.renderDialogButton(
            4,
            BUTTON_WIDTH,
            firstFiveDialogButton.x,
            thirdFiveDialogButton.y + thirdFiveDialogButton.getHeight() + 5);
        break;
      case COMPACT_TEXT_WITH_SIX_BUTTONS, TEXT_WITH_SIX_BUTTONS:
        Button firstSixDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 140);
        Button secondSixDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstSixDialogButton.x + firstSixDialogButton.getWidth() + 10,
                firstSixDialogButton.y);
        Button thirdSixDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstSixDialogButton.x,
                firstSixDialogButton.y + firstSixDialogButton.getHeight() + 5);
        this.renderDialogButton(3, BUTTON_WIDTH, secondSixDialogButton.x, thirdSixDialogButton.y);
        Button fifthSixDialogButton =
            this.renderDialogButton(
                4,
                BUTTON_WIDTH,
                firstSixDialogButton.x,
                thirdSixDialogButton.y + thirdSixDialogButton.getHeight() + 5);
        this.renderDialogButton(5, BUTTON_WIDTH, secondSixDialogButton.x, fifthSixDialogButton.y);
        break;
      default:
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
            ? this.topPos + 95
            : this.topPos + 136;

    // Forward Button
    this.dialogForwardButton =
        this.addRenderableWidget(
            new SpriteButton(
                this.leftPos + 257,
                dialogNavigationButtonTopPosition,
                12,
                12,
                Constants.TEXTURE_DIALOG,
                206,
                2,
                12,
                12,
                onPress -> {
                  this.dialogPageIndex =
                      this.dialogPageIndex < this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES
                          ? this.dialogPageIndex + 1
                          : 0;
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

    // Backward Button
    this.dialogBackwardButton =
        this.addRenderableWidget(
            new SpriteButton(
                this.leftPos + 245,
                dialogNavigationButtonTopPosition,
                12,
                12,
                Constants.TEXTURE_DIALOG,
                207,
                28,
                12,
                12,
                onPress -> {
                  this.dialogPageIndex =
                      this.dialogPageIndex > 0
                          ? this.dialogPageIndex - 1
                          : this.numberOfDialogLines / MAX_NUMBER_OF_DIALOG_LINES;
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
    super.init();

    // Default stats
    this.imageHeight = 200;
    this.imageWidth = 280;

    // Basic Position
    this.titleLabelX = 10;
    this.titleLabelY = 8;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Close Button
    this.closeButton.x = this.leftPos + this.imageWidth - 13;
    this.closeButton.y = this.topPos + 4;

    // Dialog Screen Layout
    setDialogScreenLayout(DialogUtils.getDialogScreenLayout(this.getDialogData(), this.font));
    log.debug(
        "Prepare Dialog Screen {} with page index {} for {} with {} line(s) and layout {}",
        this.getDialogUUID(),
        this.getPageIndex(),
        this.getDialogDataSet(),
        this.numberOfDialogLines,
        dialogScreenLayout);

    // Set dialog text
    this.setDialogText(this.getDialogData());
    log.debug("Dialog with {} line(s) and layout {}", this.numberOfDialogLines, dialogScreenLayout);

    // If the dialog has more than 10 lines, add a button to switch between pages.
    if (this.numberOfDialogLines > MAX_NUMBER_OF_DIALOG_LINES) {
      this.defineDialogNavigationButtons();
    }

    // Action Event for open dialog.
    if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_OPEN_DIALOG)) {
      NetworkMessageHandlerManager.getServerHandler()
          .triggerActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_OPEN_DIALOG);
    }

    // Get and render dialog buttons, if any.
    if (this.hasDialogData() && this.getDialogData().getNumberOfDialogButtons() > 0) {
      this.dialogButtons.ensureCapacity(this.getDialogData().getNumberOfDialogButtons());
      for (DialogButtonEntry dialogButtonEntry : this.getDialogData().getDialogButtons()) {
        if (dialogButtonEntry == null) {
          continue;
        }
        this.addDialogButton(dialogButtonEntry);
      }
      this.renderDialogButtons();
    }
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    if (this.getEasyNPC() == null) {
      return;
    }
    super.render(poseStack, x, y, partialTicks);

    // Render Avatar
    int avatarPositionTop = 60 + this.getEasyNPC().getEasyNPCDialogData().getEntityDialogTop();
    int left = this.leftPos + 40;
    int top = this.topPos + 70 + avatarPositionTop;
    ScreenHelper.renderEntityDialog(
        left,
        top,
        Math.round(left - 140 - (this.xMouse * 0.25)),
        Math.round(top - 120 - (this.yMouse * 0.5)),
        this.getEasyNPC());

    // Render Dialog
    renderDialog(poseStack);
  }

  // @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    Text.drawString(
        poseStack,
        this.font,
        this.title,
        this.leftPos + this.titleLabelX,
        this.topPos + this.titleLabelY);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    switch (dialogScreenLayout) {
      case UNKNOWN:
        break;
      case COMPACT_TEXT_ONLY,
          COMPACT_TEXT_WITH_ONE_BUTTON,
          COMPACT_TEXT_WITH_TWO_BUTTONS,
          COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        // Compact background
        Graphics.blit(
            poseStack, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 200, 170);
        Graphics.blit(
            poseStack, Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 200, topPos, 165, 0, 85, 170);
        break;
      default:
        // Full background
        Graphics.blit(
            poseStack, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 140);
        Graphics.blit(
            poseStack, Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 200, topPos, 165, 0, 85, 140);

        Graphics.blit(
            poseStack, Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 70, 0, 30, 210, 140);
        Graphics.blit(
            poseStack,
            Constants.TEXTURE_DEMO_BACKGROUND,
            leftPos + 200,
            topPos + 70,
            165,
            30,
            85,
            140);
    }
  }

  @Override
  public void onClose() {
    // Action Event for close dialog.
    if (this.getActionEventSet().hasActionEvent(ActionEventType.ON_CLOSE_DIALOG)) {
      NetworkMessageHandlerManager.getServerHandler()
          .triggerActionEvent(this.getEasyNPCUUID(), ActionEventType.ON_CLOSE_DIALOG);
    }
    super.onClose();
  }
}
