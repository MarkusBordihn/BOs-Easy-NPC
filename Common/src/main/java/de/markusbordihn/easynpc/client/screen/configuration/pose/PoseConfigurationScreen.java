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

  protected SliderButton createRotationSlider(
      int left, int top, ModelPart modelPart, String label) {
    return createRotationSlider(left, top, modelPart, label, false);
  }

  protected SliderButton createRotationSliderCompact(
      int left, int top, ModelPart modelPart, String label) {
    return createRotationSlider(left, top, modelPart, label, true);
  }

  protected SliderButton createRotationSlider(
      int left, int top, ModelPart modelPart, String label, boolean compact) {
    int sliderWidth = 34;
    int sliderLeftPosition = left;

    // Shift left position for compact mode ans specific model parts
    if (compact
        && (modelPart == ModelPart.BODY
            || modelPart == ModelPart.LEFT_ARM
            || modelPart == ModelPart.LEFT_LEG)) {
      sliderLeftPosition = left + 10;
    }

    // Model Part Rotation
    CustomRotation modelPartRotation = this.modelData.getModelPartRotation(modelPart);
    SliderButton sliderButtonX =
        this.addRenderableWidget(
            new SliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                label + "RotationX",
                (float) Math.toDegrees(modelPartRotation.x()),
                SliderButton.Type.DEGREE,
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
    SliderButton sliderButtonY =
        this.addRenderableWidget(
            new SliderButton(
                sliderButtonX.x + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                label + "RotationY",
                (float) Math.toDegrees(modelPartRotation.y()),
                SliderButton.Type.DEGREE,
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
    SliderButton sliderButtonZ =
        this.addRenderableWidget(
            new SliderButton(
                sliderButtonY.x + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                label + "RotationZ",
                (float) Math.toDegrees(modelPartRotation.z()),
                SliderButton.Type.DEGREE,
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

    if (compact) {
      int resetButtonLeftPosition =
          switch (modelPart) {
            case BODY, LEFT_ARM, LEFT_LEG -> left;
            default -> sliderButtonZ.x + sliderButtonZ.getWidth();
          };
      this.addRenderableWidget(
          new TextButton(
              resetButtonLeftPosition,
              top,
              10,
              Component.literal("↺"),
              button -> {
                sliderButtonX.reset();
                sliderButtonY.reset();
                sliderButtonZ.reset();
                NetworkMessageHandlerManager.getServerHandler()
                    .rotationChange(
                        this.getEasyNPCUUID(), modelPart, new CustomRotation(0f, 0f, 0f));
              }));
    } else {
      this.addRenderableWidget(
          new TextButton(
              sliderButtonX.x,
              top + 16,
              sliderWidth * 3,
              "reset",
              button -> {
                sliderButtonX.reset();
                sliderButtonY.reset();
                sliderButtonZ.reset();
                NetworkMessageHandlerManager.getServerHandler()
                    .rotationChange(
                        this.getEasyNPCUUID(), modelPart, new CustomRotation(0f, 0f, 0f));
              }));
    }
    return sliderButtonX;
  }

  protected SliderButton createPositionSlider(
      int left, int top, ModelPart modelPart, String label) {
    return createPositionSlider(left, top, modelPart, label, false);
  }

  protected SliderButton createPositionSliderCompact(
      int left, int top, ModelPart modelPart, String label) {
    return createPositionSlider(left, top, modelPart, label, true);
  }

  protected SliderButton createPositionSlider(
      int left, int top, ModelPart modelPart, String label, boolean compact) {
    int sliderWidth = 34;
    int sliderLeftPosition = left;

    // Shift left position for compact mode ans specific model parts
    if (compact
        && (modelPart == ModelPart.BODY
            || modelPart == ModelPart.LEFT_ARM
            || modelPart == ModelPart.LEFT_LEG)) {
      sliderLeftPosition = left + 10;
    }

    // Model Part Position
    CustomPosition modelPartPosition = this.modelData.getModelPartPosition(modelPart);
    SliderButton sliderButtonX =
        this.addRenderableWidget(
            new SliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                label + "PositionX",
                modelPartPosition.x(),
                SliderButton.Type.POSITION,
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
    SliderButton sliderButtonY =
        this.addRenderableWidget(
            new SliderButton(
                sliderButtonX.x + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                label + "PositionY",
                modelPartPosition.y(),
                SliderButton.Type.POSITION,
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
    SliderButton sliderButtonZ =
        this.addRenderableWidget(
            new SliderButton(
                sliderButtonY.x + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                label + "PositionZ",
                modelPartPosition.z(),
                SliderButton.Type.POSITION,
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

    if (compact) {
      int resetButtonLeftPosition =
          switch (modelPart) {
            case BODY, LEFT_ARM, LEFT_LEG -> left;
            default -> sliderButtonZ.x + sliderButtonZ.getWidth();
          };
      // Switch reset button left position for compact mode ans specific model parts
      this.addRenderableWidget(
          new TextButton(
              resetButtonLeftPosition,
              top,
              10,
              Component.literal("↺"),
              button -> {
                sliderButtonX.reset();
                sliderButtonY.reset();
                sliderButtonZ.reset();
                NetworkMessageHandlerManager.getServerHandler()
                    .modelPositionChange(
                        this.getEasyNPCUUID(), modelPart, new CustomPosition(0f, 0f, 0f));
              }));
    } else {
      this.addRenderableWidget(
          new TextButton(
              sliderButtonX.x,
              top + 20,
              sliderWidth * 3,
              "reset",
              button -> {
                sliderButtonX.reset();
                sliderButtonY.reset();
                sliderButtonZ.reset();
                NetworkMessageHandlerManager.getServerHandler()
                    .modelPositionChange(
                        this.getEasyNPCUUID(), modelPart, new CustomPosition(0f, 0f, 0f));
              }));
    }
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
