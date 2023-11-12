/*
 * Copyright 2023 Markus Bordihn
 *
 *Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 *The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 *THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package de.markusbordihn.easynpc.client.screen.dialog;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.components.SpriteButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonData;
import de.markusbordihn.easynpc.data.dialog.DialogData;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.dialog.DialogScreenLayout;
import de.markusbordihn.easynpc.data.dialog.DialogType;
import de.markusbordihn.easynpc.data.dialog.DialogUtils;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.ImageButton;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@OnlyIn(Dist.CLIENT)
public class DialogScreen extends AbstractContainerScreen<DialogMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private static final int BUTTON_WIDTH = 126;
  private static final int LARGE_BUTTON_WIDTH = 250;
  private static final int MAX_NUMBER_OF_PIXEL_PER_LINE = 180;
  private static final int MAX_NUMBER_OF_DIALOG_LINES = 10;

  // Data access
  protected final ActionEventSet actionEventDataSet;
  protected final DialogDataSet dialogDataSet;
  protected final EasyNPCEntity entity;
  protected final UUID dialogId;
  protected final UUID uuid;
  protected final int pageIndex;

  // Layout relevant Data
  protected final DialogType dialogType;
  protected final DialogData dialogData;
  // Dialog Buttons
  protected final ArrayList<Button> dialogButtons = new ArrayList<>();
  protected DialogScreenLayout dialogScreenLayout;
  // Buttons
  protected Button closeButton = null;
  protected Button dialogForwardButton = null;
  protected Button dialogBackwardButton = null;
  // Internal
  protected float xMouse;
  protected float yMouse;

  // Dialog Options
  protected String dialog;
  protected TextComponent dialogComponent;
  protected int numberOfDialogLines = 1;
  protected int dialogPageIndex = 0;
  private List<FormattedCharSequence> cachedDialogComponents = Collections.emptyList();

  public DialogScreen(DialogMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.uuid = menu.getUUID();
    this.entity = menu.getEntity();
    this.actionEventDataSet = menu.getActionEventSet();
    this.dialogDataSet = menu.getDialogDataSet();
    this.dialogType = menu.getDialogDataSet().getType();
    this.dialogId = menu.getDialogId();
    this.pageIndex = menu.getPageIndex();

    // Get relevant dialog data based on dialog id.
    if (this.dialogId != null) {
      this.dialogData = this.dialogDataSet.getDialog(this.dialogId);
    } else {
      this.dialogData = this.dialogDataSet.getDefaultDialog();
    }
  }

  protected void renderDialog(PoseStack poseStack) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DIALOG);

    // Draw dialog background bobble.
    int dialogTopPosition = topPos + 20;
    switch (this.dialogScreenLayout) {
      case COMPACT_TEXT_ONLY,
          COMPACT_TEXT_WITH_ONE_BUTTON,
          COMPACT_TEXT_WITH_TWO_BUTTONS,
          COMPACT_TEXT_WITH_THREE_BUTTONS,
          COMPACT_TEXT_WITH_FOUR_BUTTONS,
          COMPACT_TEXT_WITH_FIVE_BUTTONS,
          COMPACT_TEXT_WITH_SIX_BUTTONS:
        this.blit(poseStack, leftPos + 70, dialogTopPosition, 0, 120, 205, 78);
        break;
      default:
        this.blit(poseStack, leftPos + 70, dialogTopPosition, 0, 0, 205, 118);
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

  private void setDialogText(DialogData dialogData) {
    if (dialogData == null) {
      return;
    }
    Minecraft minecraft = this.minecraft;
    String dialogText =
        dialogData.getDialogText(entity, minecraft != null ? minecraft.player : null);
    if (dialogText == null || dialogText.isBlank()) {
      return;
    }

    // Create dialog text component.
    this.dialogComponent = new TextComponent(dialogText);

    // Split dialog text to lines.
    this.cachedDialogComponents =
        this.font.split(this.dialogComponent, MAX_NUMBER_OF_PIXEL_PER_LINE);
    this.numberOfDialogLines = Math.min(128 / font.lineHeight, this.cachedDialogComponents.size());
  }

  private Button addDialogButton(DialogButtonData dialogButtonData) {
    if (dialogButtonData == null) {
      return null;
    }

    // Create dialog button text.
    int dialogButtonMaxTextLength =
        this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_ONE_BUTTON
                || this.dialogScreenLayout == DialogScreenLayout.TEXT_WITH_ONE_BUTTON
                || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_THREE_BUTTONS
                || this.dialogScreenLayout == DialogScreenLayout.TEXT_WITH_THREE_BUTTONS
            ? 42
            : 22;
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
              if (this.actionEventDataSet.hasActionEvent(ActionEventType.ON_BUTTON_CLICK)) {
                NetworkMessageHandler.triggerActionEvent(
                    this.uuid, ActionEventType.ON_BUTTON_CLICK);
              }

              // Custom action on button click.
              if (dialogButtonData.hasActionData()) {
                UUID buttonId = dialogButtonData.getId();
                NetworkMessageHandler.triggerDialogButtonAction(this.uuid, this.dialogId, buttonId);
              } else {
                this.closeScreen();
              }
            });

    // Set dialog button visibility.
    dialogButton.visible =
        dialogButtonData.getName() != null && !dialogButtonData.getName().isBlank();

    this.dialogButtons.add(dialogButton);
    return dialogButton;
  }

  public void closeScreen() {
    Minecraft minecraft = this.minecraft;
    if (minecraft != null) {
      minecraft.setScreen(null);
    }
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
    switch (this.dialogScreenLayout) {
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
            this.dialogScreenLayout,
            this.dialogDataSet,
            this.numberOfDialogLines);
    }
  }

  @Override
  public void init() {
    if (this.entity == null) {
      return;
    }
    super.init();

    // Default stats
    this.imageHeight = 200;
    this.imageWidth = 280;

    // Basic Position
    this.titleLabelX = 10;
    this.titleLabelY = 8;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Dialog Screen Layout
    this.dialogScreenLayout = DialogUtils.getDialogScreenLayout(this.dialogData, this.font);
    log.info(
        "Prepare Dialog Screen {} with page index {} for {} with {} line(s) and layout {}",
        this.dialogId,
        this.pageIndex,
        this.dialogDataSet,
        this.numberOfDialogLines,
        this.dialogScreenLayout);

    // Close Button
    this.closeButton =
        this.addRenderableWidget(
            new ImageButton(
                this.leftPos + this.imageWidth - 15,
                this.topPos + 6,
                10,
                10,
                64,
                38,
                Constants.TEXTURE_CONFIGURATION,
                onPress -> closeScreen()));

    // Set dialog text
    this.setDialogText(this.dialogData);
    log.debug(
        "Dialog with {} line(s) and layout {}", this.numberOfDialogLines, this.dialogScreenLayout);

    // If the dialog has more than 10 lines, add a button to switch between pages.
    if (this.numberOfDialogLines > MAX_NUMBER_OF_DIALOG_LINES) {

      int dialogNavigationButtonTopPosition =
          this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_ONLY
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_ONE_BUTTON
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_TWO_BUTTONS
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_THREE_BUTTONS
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_FOUR_BUTTONS
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_FIVE_BUTTONS
                  || this.dialogScreenLayout == DialogScreenLayout.COMPACT_TEXT_WITH_SIX_BUTTONS
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

    // Action Event for open dialog.
    if (this.actionEventDataSet.hasActionEvent(ActionEventType.ON_OPEN_DIALOG)) {
      NetworkMessageHandler.triggerActionEvent(this.uuid, ActionEventType.ON_OPEN_DIALOG);
    }

    // Get dialog buttons, if any.
    if (this.dialogData.getNumberOfButtons() > 0) {
      this.dialogButtons.ensureCapacity(this.dialogData.getNumberOfButtons());
      for (DialogButtonData dialogButtonData : this.dialogData.getButtons()) {
        if (dialogButtonData == null) {
          continue;
        }
        this.addDialogButton(dialogButtonData);
      }
    }

    // Render dialog buttons.
    this.renderDialogButtons();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    if (this.entity == null) {
      return;
    }

    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Render Avatar
    int avatarPositionTop = 60 + this.entity.getEntityDialogTop();
    int left = this.leftPos + 40;
    int top = this.topPos + 70 + avatarPositionTop;
    ScreenHelper.renderEntityDialog(
        left,
        top,
        Math.round(left - 140 - (this.xMouse * 0.25)),
        Math.round(top - 120 - (this.yMouse * 0.5)),
        this.entity);

    // Render Dialog
    renderDialog(poseStack);
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    Text.drawString(poseStack, this.font, this.title, this.titleLabelX, this.titleLabelY);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    switch (this.dialogScreenLayout) {
      case COMPACT_TEXT_ONLY, COMPACT_TEXT_WITH_ONE_BUTTON, COMPACT_TEXT_WITH_TWO_BUTTONS:
        // Compact background
        this.blit(poseStack, leftPos, topPos, 0, 0, 200, 170);
        this.blit(poseStack, leftPos + 200, topPos, 165, 0, 85, 170);
        break;
      default:
        // Full background
        this.blit(poseStack, leftPos, topPos, 0, 0, 210, 140);
        this.blit(poseStack, leftPos + 200, topPos, 165, 0, 85, 140);

        this.blit(poseStack, leftPos, topPos + 70, 0, 30, 210, 140);
        this.blit(poseStack, leftPos + 200, topPos + 70, 165, 30, 85, 140);
    }
  }

  @Override
  public void onClose() {
    // Action Event for close dialog.
    if (this.actionEventDataSet.hasActionEvent(ActionEventType.ON_CLOSE_DIALOG)) {
      NetworkMessageHandler.triggerActionEvent(this.uuid, ActionEventType.ON_CLOSE_DIALOG);
    }
    super.onClose();
  }
}
