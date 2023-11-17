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

package de.markusbordihn.easynpc.client.screen.configuration.attribute;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.menu.configuration.attribute.BasicAttributeConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

@OnlyIn(Dist.CLIENT)
public class BasicAttributeConfigurationScreen
    extends AttributeConfigurationScreen<BasicAttributeConfigurationMenu> {

  // Checkboxes

  // Text

  // Cache

  public BasicAttributeConfigurationScreen(
      BasicAttributeConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicAttributeButton.active = false;

    // Checkboxes

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos,
            this.buttonTopPos + 25,
            "Float",
            this.entity.getAttributeCanFloat(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.CAN_FLOAT, checkbox.selected());
            }));

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos,
            this.buttonTopPos + 45,
            "Open Door",
            this.entity.getAttributeCanOpenDoor(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.CAN_OPEN_DOOR, checkbox.selected());
            }));

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos + 90,
            this.buttonTopPos + 45,
            "Close Door",
            this.entity.getAttributeCanCloseDoor(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.CAN_CLOSE_DOOR, checkbox.selected());
            }));

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos + 180,
            this.buttonTopPos + 45,
            "Pass Door",
            this.entity.getAttributeCanPassDoor(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.CAN_PASS_DOOR, checkbox.selected());
            }));

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos,
            this.buttonTopPos + 65,
            "Attackable",
            this.entity.getAttributeIsAttackable(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.IS_ATTACKABLE, checkbox.selected());
            }));

    this.addRenderableWidget(
        new Checkbox(
            this.buttonLeftPos + 90,
            this.buttonTopPos + 65,
            "Pushable",
            this.entity.getAttributeIsPushable(),
            checkbox -> {
              NetworkMessageHandler.entityAttributeChange(
                  uuid, EntityAttribute.IS_PUSHABLE, checkbox.selected());
            }));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
  }
}
