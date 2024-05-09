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

import com.mojang.blaze3d.systems.RenderSystem;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.EasyNPCScreen;
import de.markusbordihn.easynpc.client.screen.components.CloseButton;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogScreenLayout;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import de.markusbordihn.easynpc.screen.ScreenHelper;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DialogScreen<T extends EasyNPCMenu> extends EasyNPCScreen<T> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int BUTTON_WIDTH = 126;
  private static final int MIDDLE_BUTTON_WIDTH = 200;
  private static final int LARGE_BUTTON_WIDTH = 250;
  private static final int MAX_NUMBER_OF_PIXEL_PER_LINE = 180;
  private static final int MAX_NUMBER_OF_DIALOG_LINES = 10;
  private static DialogScreenLayout dialogScreenLayout = DialogScreenLayout.UNKNOWN;
  protected final ArrayList<Button> dialogButtons = new ArrayList<>();
  protected Button closeButton = null;
  protected Button dialogForwardButton = null;
  protected Button dialogBackwardButton = null;
  protected String dialog;
  protected Component dialogComponent;
  protected int numberOfDialogLines = 1;
  protected int dialogPageIndex = 0;
  private List<FormattedCharSequence> cachedDialogComponents = Collections.emptyList();

  public DialogScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private static void setDialogScreenLayout(DialogScreenLayout dialogScreenLayout) {
    DialogScreen.dialogScreenLayout = dialogScreenLayout;
  }

  protected void renderDialog(GuiGraphics guiGraphics) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DIALOG);

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
        guiGraphics.blit(
            Constants.TEXTURE_DIALOG, leftPos + 70, dialogTopPosition, 0, 120, 205, 78);
        break;
      default:
        guiGraphics.blit(Constants.TEXTURE_DIALOG, leftPos + 70, dialogTopPosition, 0, 0, 205, 118);
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
            guiGraphics, this.font, formattedCharSequence, leftPos + 87, textTopPosition, 0);
      }
    }
  }

  private void setDialogText(DialogDataEntry dialogData) {
    if (dialogData == null) {
      return;
    }
    Minecraft minecraft = this.minecraft;
    String dialogText =
        dialogData.getDialogText(
            this.getEasyNPC().getLivingEntity(), minecraft != null ? minecraft.player : null);
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

  private void addDialogButton(DialogButtonData dialogButtonData) {
    if (dialogButtonData == null) {
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
    Component dialogButtonText = dialogButtonData.getButtonName(dialogButtonMaxTextLength);

    // Create dialog button.
    TextButton dialogButton =
        new TextButton(
            this.leftPos + 70,
            this.topPos + 55,
            198,
            dialogButtonText,
            onPress -> {
              // Action Event on button click.
              if (this.hasActionEventSet()
                  && this.getActionEventSet().hasActionEvent(ActionEventType.ON_BUTTON_CLICK)) {
                networkMessageHandler.triggerActionEvent(
                    this.getNpcUUID(), ActionEventType.ON_BUTTON_CLICK);
              }

              // Custom action on button click.
              if (dialogButtonData.hasActionData()) {
                UUID buttonId = dialogButtonData.getId();
                networkMessageHandler.triggerDialogButtonAction(
                    this.getNpcUUID(), this.getDialogUUID(), buttonId);
              } else {
                this.closeScreen();
              }
            });

    // Set dialog button visibility.
    dialogButton.visible =
        dialogButtonData.getName() != null && !dialogButtonData.getName().isBlank();

    this.dialogButtons.add(dialogButton);
  }

  private Button renderDialogButton(int buttonIndex, int width, int left, int top) {
    Button dialogButton = this.dialogButtons.get(buttonIndex);
    dialogButton.setWidth(width);
    dialogButton.setX(left);
    dialogButton.setY(top);
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
            firstCompactDialogButton.getX() + firstCompactDialogButton.getWidth() + 10,
            firstCompactDialogButton.getY());
        break;
      case COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        Button firstCompactLargeDialogButton =
            this.renderDialogButton(0, MIDDLE_BUTTON_WIDTH, this.leftPos + 75, this.topPos + 115);
        this.renderDialogButton(
            1,
            MIDDLE_BUTTON_WIDTH,
            firstCompactLargeDialogButton.getX(),
            firstCompactLargeDialogButton.getY() + firstCompactLargeDialogButton.getHeight() + 10);
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
            firstTwoDialogButton.getX(),
            firstTwoDialogButton.getY() + firstTwoDialogButton.getHeight() + 10);
        break;
      case COMPACT_TEXT_WITH_THREE_BUTTONS, TEXT_WITH_THREE_BUTTONS:
        Button firstThreeDialogButton =
            this.renderDialogButton(0, LARGE_BUTTON_WIDTH, this.leftPos + 18, this.topPos + 140);
        Button secondThreeDialogButton =
            this.renderDialogButton(
                1,
                LARGE_BUTTON_WIDTH,
                firstThreeDialogButton.getX(),
                firstThreeDialogButton.getY() + firstThreeDialogButton.getHeight() + 5);
        this.renderDialogButton(
            2,
            LARGE_BUTTON_WIDTH,
            secondThreeDialogButton.getX(),
            secondThreeDialogButton.getY() + secondThreeDialogButton.getHeight() + 5);
        break;
      case COMPACT_TEXT_WITH_FOUR_BUTTONS, TEXT_WITH_FOUR_BUTTONS:
        Button firstFourDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 150);
        Button secondFourDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstFourDialogButton.getX() + firstFourDialogButton.getWidth() + 10,
                firstFourDialogButton.getY());
        Button thirdFourDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstFourDialogButton.getX(),
                firstFourDialogButton.getY() + firstFourDialogButton.getHeight() + 10);
        this.renderDialogButton(
            3, BUTTON_WIDTH, secondFourDialogButton.getX(), thirdFourDialogButton.getY());
        break;
      case COMPACT_TEXT_WITH_FIVE_BUTTONS, TEXT_WITH_FIVE_BUTTONS:
        Button firstFiveDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 140);
        Button secondFiveDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstFiveDialogButton.getX() + firstFiveDialogButton.getWidth() + 10,
                firstFiveDialogButton.getY());
        Button thirdFiveDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstFiveDialogButton.getX(),
                firstFiveDialogButton.getY() + firstFiveDialogButton.getHeight() + 5);
        this.renderDialogButton(
            3, BUTTON_WIDTH, secondFiveDialogButton.getX(), thirdFiveDialogButton.getY());
        this.renderDialogButton(
            4,
            BUTTON_WIDTH,
            firstFiveDialogButton.getX(),
            thirdFiveDialogButton.getY() + thirdFiveDialogButton.getHeight() + 5);
        break;
      case COMPACT_TEXT_WITH_SIX_BUTTONS, TEXT_WITH_SIX_BUTTONS:
        Button firstSixDialogButton =
            this.renderDialogButton(0, BUTTON_WIDTH, this.leftPos + 10, this.topPos + 140);
        Button secondSixDialogButton =
            this.renderDialogButton(
                1,
                BUTTON_WIDTH,
                firstSixDialogButton.getX() + firstSixDialogButton.getWidth() + 10,
                firstSixDialogButton.getY());
        Button thirdSixDialogButton =
            this.renderDialogButton(
                2,
                BUTTON_WIDTH,
                firstSixDialogButton.getX(),
                firstSixDialogButton.getY() + firstSixDialogButton.getHeight() + 5);
        this.renderDialogButton(
            3, BUTTON_WIDTH, secondSixDialogButton.getX(), thirdSixDialogButton.getY());
        Button fifthSixDialogButton =
            this.renderDialogButton(
                4,
                BUTTON_WIDTH,
                firstSixDialogButton.getX(),
                thirdSixDialogButton.getY() + thirdSixDialogButton.getHeight() + 5);
        this.renderDialogButton(
            5, BUTTON_WIDTH, secondSixDialogButton.getX(), fifthSixDialogButton.getY());
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
  }

  @Override
  public void defineScreen() {
    super.defineScreen();

    // Dialog Data
    DialogDataEntry dialogData = this.getDialogData();

    // Dialog Screen Layout
    setDialogScreenLayout(DialogUtils.getDialogScreenLayout(this.getDialogData(), this.font));
    log.debug(
        "Prepare Dialog Screen {} with page index {} for {} with {} line(s) and layout {}",
        this.getDialogUUID(),
        this.getPageIndex(),
        this.getDialogDataSet(),
        this.numberOfDialogLines,
        dialogScreenLayout);

    // Close Button
    this.closeButton =
        this.addRenderableWidget(
            new CloseButton(
                this.leftPos + this.imageWidth - 13, this.topPos + 4, onPress -> closeScreen()));

    // Set dialog text
    this.setDialogText(dialogData);
    log.debug("Dialog with {} line(s) and layout {}", this.numberOfDialogLines, dialogScreenLayout);

    // If the dialog has more than 10 lines, add a button to switch between pages.
    if (this.numberOfDialogLines > MAX_NUMBER_OF_DIALOG_LINES) {
      this.defineDialogNavigationButtons();
    }

    // Action Event for open dialog.
    if (this.hasActionEventSet()
        && this.getActionEventSet().hasActionEvent(ActionEventType.ON_OPEN_DIALOG)) {
      networkMessageHandler.triggerActionEvent(this.getNpcUUID(), ActionEventType.ON_OPEN_DIALOG);
    }

    // Get and render dialog buttons, if any.
    if (this.hasDialogDataSet() && dialogData.getNumberOfButtons() > 0) {
      this.dialogButtons.ensureCapacity(dialogData.getNumberOfButtons());
      for (DialogButtonData dialogButtonData : dialogData.getButtons()) {
        if (dialogButtonData == null) {
          continue;
        }
        this.addDialogButton(dialogButtonData);
      }
      this.renderDialogButtons();
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    if (this.getEasyNPC() == null) {
      return;
    }
    super.render(guiGraphics, x, y, partialTicks);

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
    renderDialog(guiGraphics);
  }

  @Override
  protected void renderLabels(GuiGraphics guiGraphics, int x, int y) {
    Text.drawString(guiGraphics, this.font, this.title, this.titleLabelX, this.titleLabelY);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    switch (dialogScreenLayout) {
      case UNKNOWN:
        break;
      case COMPACT_TEXT_ONLY,
          COMPACT_TEXT_WITH_ONE_BUTTON,
          COMPACT_TEXT_WITH_TWO_BUTTONS,
          COMPACT_TEXT_WITH_TWO_LARGE_BUTTONS:
        // Compact background
        guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 200, 170);
        guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 200, topPos, 165, 0, 85, 170);
        break;
      default:
        // Full background
        guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 210, 140);
        guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 200, topPos, 165, 0, 85, 140);

        guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 70, 0, 30, 210, 140);
        guiGraphics.blit(
            Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 200, topPos + 70, 165, 30, 85, 140);
    }
  }

  @Override
  public void onClose() {
    // Action Event for close dialog.
    if (this.hasActionEventSet()
        && this.getActionEventSet().hasActionEvent(ActionEventType.ON_CLOSE_DIALOG)) {
      networkMessageHandler.triggerActionEvent(this.getNpcUUID(), ActionEventType.ON_CLOSE_DIALOG);
    }
    super.onClose();
  }
}
