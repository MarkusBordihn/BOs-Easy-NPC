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

package de.markusbordihn.easynpc.menu.configuration.attribute;

import de.markusbordihn.easynpc.menu.ModMenuTypes;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

public class BaseAttributeConfigurationMenu extends AttributeConfigurationMenu {

  private final double followRange;
  private final double knockbackResistance;
  private final double attackDamage;
  private final double attackKnockback;

  public BaseAttributeConfigurationMenu(
      int windowId,
      Inventory playerInventory,
      UUID uuid,
      double followRange,
      double knockbackResistance,
      double attackDamage,
      double attackKnockback) {
    super(ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU.get(), windowId, playerInventory, uuid);
    this.followRange = followRange;
    this.knockbackResistance = knockbackResistance;
    this.attackDamage = attackDamage;
    this.attackKnockback = attackKnockback;
  }

  public BaseAttributeConfigurationMenu(
      int windowId, Inventory playerInventory, FriendlyByteBuf data) {
    this(
        windowId,
        playerInventory,
        data.readUUID(),
        data.readDouble(),
        data.readDouble(),
        data.readDouble(),
        data.readDouble());
  }

  public static MenuProvider getMenuProvider(UUID uuid, LivingEntity entity) {
    return new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Base Attribute for " + entity.getName().getString());
      }

      @Override
      public AbstractContainerMenu createMenu(
          int windowId, Inventory inventory, Player serverPlayer) {
        double followRangeValue =
            entity.getAttribute(Attributes.FOLLOW_RANGE) != null
                ? entity.getAttribute(Attributes.FOLLOW_RANGE).getBaseValue()
                : 32.0d;
        double knockbackResistanceValue =
            entity.getAttribute(Attributes.KNOCKBACK_RESISTANCE) != null
                ? entity.getAttribute(Attributes.KNOCKBACK_RESISTANCE).getBaseValue()
                : 0.0d;
        double attackDamageValue =
            entity.getAttribute(Attributes.ATTACK_DAMAGE) != null
                ? entity.getAttribute(Attributes.ATTACK_DAMAGE).getBaseValue()
                : 2.0d;
        double attackKnockbackValue =
            entity.getAttribute(Attributes.ATTACK_KNOCKBACK) != null
                ? entity.getAttribute(Attributes.ATTACK_KNOCKBACK).getBaseValue()
                : 0.0d;
        return new BaseAttributeConfigurationMenu(
            windowId,
            inventory,
            uuid,
            followRangeValue,
            knockbackResistanceValue,
            attackDamageValue,
            attackKnockbackValue);
      }
    };
  }

  public double getFollowRange() {
    return this.followRange;
  }

  public double getKnockbackResistance() {
    return this.knockbackResistance;
  }

  public double getAttackDamage() {
    return this.attackDamage;
  }

  public double getAttackKnockback() {
    return this.attackKnockback;
  }
}
