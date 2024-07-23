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

package de.markusbordihn.easynpc.item.configuration;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.menu.MenuManager;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EasyNPCWandItem extends Item {

  public static final String ID = "easy_npc_wand";
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final int GLOWING_DURATION = 4 * 20;

  public EasyNPCWandItem(Properties properties) {
    super(properties);
  }

  @Override
  public void inventoryTick(
      ItemStack itemStack, Level level, Entity entity, int slot, boolean selected) {
    // Highlight all nearby EasyNPC entities
    if (selected && entity instanceof Player player) {
      for (EasyNPCBaseEntity<?> easyNPCEntity :
          level.getEntitiesOfClass(
              EasyNPCBaseEntity.class, player.getBoundingBox().inflate(0.5), Entity::isAlive)) {
        if (easyNPCEntity.hasEffect(MobEffects.GLOWING)) {
          MobEffectInstance mobEffect = easyNPCEntity.getEffect(MobEffects.GLOWING);
          if (mobEffect != null && mobEffect.getDuration() < 5) {
            easyNPCEntity.addEffect(
                new MobEffectInstance(MobEffects.GLOWING, GLOWING_DURATION, 1, false, false));
          }
        } else {
          easyNPCEntity.addEffect(
              new MobEffectInstance(MobEffects.GLOWING, GLOWING_DURATION, 1, false, false));
        }
      }
    }
  }

  @Override
  public InteractionResult useOn(UseOnContext userContext) {
    Level level = userContext.getLevel();
    Player player = userContext.getPlayer();
    if (player instanceof ServerPlayer serverPlayer) {
      BlockPos blockPos = userContext.getClickedPos();

      // 1. Search all nearby EasyNPC entities above and below the block position.
      AABB aabbAbove =
          new AABB(
              blockPos.getX() - 0.25d,
              blockPos.getY() - 2d,
              blockPos.getZ() - 0.25d,
              blockPos.getX() + 0.25d,
              blockPos.getY() + 2d,
              blockPos.getZ() + 0.25d);
      for (EasyNPCBaseEntity<?> easyNPCEntity :
          level.getEntitiesOfClass(
              EasyNPCBaseEntity.class, aabbAbove.inflate(0.5), Entity::isAlive)) {
        if (easyNPCEntity != null) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPCEntity, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // 2. Search all nearby EasyNPC entities around the block position.
      AABB aabbAround =
          new AABB(
              blockPos.getX() - 0.5d,
              blockPos.getY() - 0.5d,
              blockPos.getZ() - 0.5d,
              blockPos.getX() + 1d,
              blockPos.getY() + 1d,
              blockPos.getZ() + 1d);
      for (EasyNPCBaseEntity<?> easyNPCEntity :
          level.getEntitiesOfClass(
              EasyNPCBaseEntity.class, aabbAround.inflate(0.5), Entity::isAlive)) {
        if (easyNPCEntity != null) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPCEntity, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // 3. Expand the search area by 2.5x to find all nearby EasyNPC entities.
      for (EasyNPCBaseEntity<?> easyNPCEntity :
          level.getEntitiesOfClass(
              EasyNPCBaseEntity.class, aabbAround.inflate(2.5), Entity::isAlive)) {
        if (easyNPCEntity != null) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPCEntity, 0);
          return InteractionResult.SUCCESS;
        }
      }
    }
    return InteractionResult.PASS;
  }

  @Override
  public boolean isFoil(ItemStack itemStack) {
    return true;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack,
      TooltipContext tooltipContext,
      List<Component> tooltipList,
      TooltipFlag tooltipFlag) {
    // Display description.
    tooltipList.add(Component.translatable(Constants.TEXT_ITEM_PREFIX + ID));
  }
}
