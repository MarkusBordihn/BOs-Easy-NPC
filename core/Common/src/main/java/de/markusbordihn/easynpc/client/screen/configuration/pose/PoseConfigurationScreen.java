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

package de.markusbordihn.easynpc.client.screen.configuration.pose;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.model.ModelPart;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.components.TextComponent;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class PoseConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  protected final ModelData<?> modelData;
  protected Button defaultPoseButton;
  protected Button advancedPoseButton;
  protected Button customPoseButton;

  public PoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.modelData = this.getEasyNPC().getEasyNPCModelData();
  }

  protected RangeSliderButton createRotationSlider(
      int left, int top, ModelPart modelPart, String label) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftPosition = left + 10;

    // Model Part Rotation
    CustomRotation modelPartRotation = this.modelData.getModelPartRotation(modelPart);
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.x()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .rotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              (float) Math.toRadians(slider.getTargetValue()),
                              currentModelPartRotation.y(),
                              currentModelPartRotation.z()));
                }));
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.x + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.y()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .rotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              currentModelPartRotation.x(),
                              (float) Math.toRadians(slider.getTargetValue()),
                              currentModelPartRotation.z()));
                }));
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.x + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.z()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .rotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              currentModelPartRotation.x(),
                              currentModelPartRotation.y(),
                              (float) Math.toRadians(slider.getTargetValue())));
                }));

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
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

    // Slider reset button
    int resetButtonLeftPosition = sliderButtonZ.x + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftPosition,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              NetworkMessageHandlerManager.getServerHandler()
                  .rotationChange(this.getEasyNPCUUID(), modelPart, new CustomRotation(0f, 0f, 0f));
            }));

    return sliderButtonX;
  }

  protected RangeSliderButton createPositionSliderCompact(
      int left, int top, ModelPart modelPart, String label) {
    return createPositionSlider(left, top, modelPart, label, true);
  }

  protected RangeSliderButton createPositionSlider(
      int left, int top, ModelPart modelPart, String label, boolean compact) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftPosition = left + 10;

    // Model Part Position.
    CustomPosition modelPartPosition = this.modelData.getModelPartPosition(modelPart);
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.x(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomPosition(
                              slider.getTargetValue(),
                              currentModelPartPosition.y(),
                              currentModelPartPosition.z()));
                }));
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.x + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.y(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomPosition(
                              currentModelPartPosition.x(),
                              slider.getTargetValue(),
                              currentModelPartPosition.z()));
                }));
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.x + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.z(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomPosition(
                              currentModelPartPosition.x(),
                              currentModelPartPosition.y(),
                              slider.getTargetValue()));
                }));

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
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

    // Slider reset button
    int resetButtonLeftPosition = sliderButtonZ.x + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftPosition,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              NetworkMessageHandlerManager.getServerHandler()
                  .modelPositionChange(
                      this.getEasyNPCUUID(), modelPart, new CustomPosition(0f, 0f, 0f));
            }));

    return sliderButtonX;
  }

  @Override
  public void init() {
    super.init();

    // Pose Types
    int poseButtonWidth = 80;
    this.defaultPoseButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                poseButtonWidth - 10,
                "default",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DEFAULT_POSE)));

    this.advancedPoseButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos + this.defaultPoseButton.getWidth(),
                this.buttonTopPos,
                poseButtonWidth + 10,
                "advanced",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.ADVANCED_POSE)));

    this.customPoseButton =
        this.addRenderableWidget(
            new TextButton(
                advancedPoseButton.x + advancedPoseButton.getWidth(),
                this.buttonTopPos,
                poseButtonWidth + 20,
                "custom",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.CUSTOM_POSE)));
  }
}
