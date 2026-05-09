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

package de.markusbordihn.easynpc.configui.client.screen.configuration.rotation;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DefaultRotationConfigurationScreen<T extends ConfigurationMenu>
    extends RotationConfigurationScreen<T> {

  protected Checkbox rootRotationCheckbox;

  public DefaultRotationConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.showCloseButton = true;
    this.renderBackground = false;
  }

  private void sendRotationUpdate(float x, float y, float z) {
    ModelDataCapable<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    boolean locked = modelData.getModelRootData().rotation().locked();
    NetworkMessageHandlerManager.getServerHandler()
        .modelRotationChange(
            this.getEasyNPCUUID(), ModelPartType.ROOT, new CustomRotation(x, y, z, locked));
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultRotationButton.active = false;

    // Root Rotations
    ModelDataCapable<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    CustomRotation rootRotation = modelData.getModelRootData().rotation();

    int sliderWidth = 80;
    int sliderHeight = 16;
    int sliderLeftPosition = this.contentLeftPos + 10;
    int sliderTopPosition = this.contentTopPos;

    // Root Rotation X
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftPosition,
                sliderTopPosition,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(rootRotation.x()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation current = modelData.getModelRootData().rotation();
                  sendRotationUpdate(
                      (float) Math.toRadians(slider.getTargetValue()), current.y(), current.z());
                }));

    // Root Rotation Y
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.getX() + sliderButtonX.getWidth(),
                sliderTopPosition,
                sliderWidth,
                sliderHeight,
                rootRotation.y(),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation current = modelData.getModelRootData().rotation();
                  sendRotationUpdate(current.x(), slider.getTargetValue(), current.z());
                }));

    // Root Rotation Z
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.getX() + sliderButtonY.getWidth(),
                sliderTopPosition,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(rootRotation.z()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation current = modelData.getModelRootData().rotation();
                  sendRotationUpdate(
                      current.x(), current.y(), (float) Math.toRadians(slider.getTargetValue()));
                }));

    // Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            this.contentLeftPos,
            sliderTopPosition,
            10,
            RangeSliderButton.EDIT_TEXT,
            button -> {
              if (button.getMessage() == RangeSliderButton.EDIT_TEXT) {
                sliderButtonX.showTextField();
                sliderButtonY.showTextField();
                sliderButtonZ.showTextField();
                button.setMessage(RangeSliderButton.DONE_TEXT);
              } else {
                sliderButtonX.showSliderButton();
                sliderButtonY.showSliderButton();
                sliderButtonZ.showSliderButton();
                button.setMessage(RangeSliderButton.EDIT_TEXT);
              }
            }));

    // Reset Button
    int resetButtonLeftPosition = sliderButtonZ.getX() + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftPosition,
            sliderTopPosition,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              sendRotationUpdate(0, 0, 0);
            }));

    // Lock Root Rotation Checkbox
    this.rootRotationCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                resetButtonLeftPosition + 15,
                sliderTopPosition + 1,
                "lock_rotation",
                modelData.getModelRootData().rotation().locked(),
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .modelRotationChange(
                            this.getEasyNPCUUID(),
                            ModelPartType.ROOT,
                            modelData
                                .getModelRootData()
                                .rotation()
                                .withLocked(checkbox.selected()))));
    this.rootRotationCheckbox.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("lock_rotation.tooltip")));
  }

  @Override
  public void updateTick() {
    super.updateTick();

    ModelDataCapable<?> modelData = this.getEasyNPC().getEasyNPCModelData();
    if (modelData != null) {
      CustomRotation rootRotation = modelData.getModelRootData().rotation();
      this.rootRotationCheckbox.setSelected(rootRotation.locked());
    }
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Rotation axis labels
    int labelYPosition = this.contentTopPos + 19;
    Text.drawString(guiGraphics, this.font, "X", this.contentLeftPos + 45, labelYPosition);
    Text.drawString(guiGraphics, this.font, "Y", this.contentLeftPos + 125, labelYPosition);
    Text.drawString(guiGraphics, this.font, "Z", this.contentLeftPos + 210, labelYPosition);
  }
}
