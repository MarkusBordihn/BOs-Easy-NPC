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

package de.markusbordihn.easynpc.client.screen.configuration.position;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.phys.Vec3;

public class DefaultPositionConfigurationScreen<T extends ConfigurationMenu>
    extends PositionConfigurationScreen<T> {

  private static final float POSITION_STEPS = 0.5f;
  protected EditBox positionXBox;
  protected EditBox positionYBox;
  protected EditBox positionZBox;
  protected Checkbox positionFreefallCheckbox;
  protected double positionX = 0.0D;
  protected double positionY = 0.0D;
  protected double positionZ = 0.0D;
  protected Button positionXMinusButton;
  protected Button positionXPlusButton;
  protected Button positionYMinusButton;
  protected Button positionYPlusButton;
  protected Button positionZMinusButton;
  protected Button positionZPlusButton;

  public DefaultPositionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.showCloseButton = true;
    this.renderBackground = false;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultPositionButton.active = false;

    // Position Coordinates
    Vec3 entityPosition = this.getEasyNPCEntity().position();
    this.positionX = entityPosition.x;
    this.positionY = entityPosition.y;
    this.positionZ = entityPosition.z;

    // Define Positions
    int positionTopPos = this.contentTopPos + 10;

    // X Position
    this.positionXBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 15, positionTopPos, 60));
    this.positionXBox.setMaxLength(8);
    this.positionXBox.setValue(String.valueOf(this.positionX));
    this.positionXBox.setResponder(
        consumer -> {
          Double newPositionX = getDoubleValue(this.positionXBox.getValue());
          if (newPositionX != null) {
            this.positionX = newPositionX;
            NetworkMessageHandlerManager.getServerHandler()
                .positionChange(
                    this.getNpcUUID(), new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionXMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionXBox.x - 15,
                this.positionXBox.y,
                15,
                "-",
                button -> {
                  this.positionX -= POSITION_STEPS;
                  this.positionXBox.setValue(String.valueOf(this.positionX));
                }));
    this.positionXPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionXBox.x + this.positionXBox.getWidth() + 1,
                this.positionXBox.y,
                15,
                "+",
                button -> {
                  this.positionX += POSITION_STEPS;
                  this.positionXBox.setValue(String.valueOf(this.positionX));
                }));

    // Y Position
    this.positionYBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 111, positionTopPos, 60));
    this.positionYBox.setMaxLength(8);
    this.positionYBox.setValue(String.valueOf(this.positionY));
    this.positionYBox.setResponder(
        consumer -> {
          Double newPositionY = getDoubleValue(this.positionYBox.getValue());
          if (newPositionY != null) {
            this.positionY = newPositionY;
            NetworkMessageHandlerManager.getServerHandler()
                .positionChange(
                    this.getNpcUUID(), new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionYMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionYBox.x - 15,
                this.positionYBox.y,
                15,
                Component.literal("-"),
                button -> {
                  this.positionY -= POSITION_STEPS;
                  this.positionYBox.setValue(String.valueOf(this.positionY));
                }));
    this.positionYPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionYBox.x + this.positionYBox.getWidth() + 1,
                this.positionYBox.y,
                15,
                Component.literal("+"),
                button -> {
                  this.positionY += POSITION_STEPS;
                  this.positionYBox.setValue(String.valueOf(this.positionY));
                }));

    // Z Position
    this.positionZBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 207, positionTopPos, 60));
    this.positionZBox.setMaxLength(8);
    this.positionZBox.setValue(String.valueOf(this.positionZ));
    this.positionZBox.setResponder(
        consumer -> {
          Double newPositionZ = getDoubleValue(this.positionZBox.getValue());
          if (newPositionZ != null) {
            this.positionZ = newPositionZ;
            NetworkMessageHandlerManager.getServerHandler()
                .positionChange(
                    this.getNpcUUID(), new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionZMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionZBox.x - 15,
                this.positionZBox.y,
                15,
                Component.literal("-"),
                button -> {
                  this.positionZ -= POSITION_STEPS;
                  this.positionZBox.setValue(String.valueOf(this.positionZ));
                }));
    this.positionZPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionZBox.x + this.positionZBox.getWidth() + 1,
                this.positionZBox.y,
                15,
                Component.literal("+"),
                button -> {
                  this.positionZ += POSITION_STEPS;
                  this.positionZBox.setValue(String.valueOf(this.positionZ));
                }));

    // Freefall Checkbox
    AttributeData<?> attributeData = this.getEasyNPC().getEasyNPCAttributeData();
    this.positionFreefallCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 200,
                positionTopPos + 20,
                "free_fall",
                attributeData.getAttributeFreefall(),
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .entityAttributeChange(
                            this.getNpcUUID(), EntityAttribute.FREEFALL, checkbox.selected())));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Position Text
    Text.drawString(
        poseStack, this.font, "Position X", this.positionXBox.x + 5, this.positionXBox.y - 10);
    Text.drawString(
        poseStack, this.font, "Position Y", this.positionYBox.x + 5, this.positionYBox.y - 10);
    Text.drawString(
        poseStack, this.font, "Position Z", this.positionZBox.x + 5, this.positionZBox.y - 10);
  }
}
