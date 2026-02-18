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

package de.markusbordihn.easynpc.configui.item.configuration;

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.menu.MenuManager;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
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
  private static final double HIGHLIGHT_RADIUS = 32.0d;

  public EasyNPCWandItem(Properties properties) {
    super(properties);
  }

  private void highlightEasyNPC(EasyNPCBase<?> easyNPC) {
    if (easyNPC instanceof Mob mob) {
      mob.addEffect(
          new MobEffectInstance(MobEffects.GLOWING, GLOWING_DURATION, 0, false, false, true));
    }
  }

  @Override
  public void inventoryTick(
      ItemStack itemStack, Level level, Entity entity, int slot, boolean selected) {
    if (selected
        && level instanceof ServerLevel serverLevel
        && entity instanceof ServerPlayer serverPlayer
        && !(serverPlayer.containerMenu instanceof ConfigurationMenu)
        && level.getGameTime() % 30 == 0) {
      AABB searchArea = serverPlayer.getBoundingBox().inflate(HIGHLIGHT_RADIUS);
      for (Mob pathfinderMob :
          serverLevel.getEntitiesOfClass(
              Mob.class, searchArea, mob -> mob.isAlive() && mob instanceof EasyNPCBase<?>)) {
        highlightEasyNPC((EasyNPCBase<?>) pathfinderMob);
      }
    }
  }

  @Override
  public InteractionResult interactLivingEntity(
      ItemStack itemStack,
      Player player,
      LivingEntity livingEntity,
      InteractionHand interactionHand) {
    if (player instanceof ServerPlayer serverPlayer
        && livingEntity instanceof EasyNPCBase<?> easyNPCEntity) {

      // Shift+right-click: Quick rotate NPC to face the player.
      if (player.isShiftKeyDown()) {
        ModelDataCapable<?> modelData = easyNPCEntity.getEasyNPCModelData();
        if (modelData != null) {
          double dx = player.getX() - livingEntity.getX();
          double dz = player.getZ() - livingEntity.getZ();
          float yaw = (float) Math.toDegrees(Mth.atan2(dz, dx)) - 90.0f;
          yaw = Mth.wrapDegrees(yaw);
          modelData.setModelRotation(yaw);
          serverPlayer.displayClientMessage(
              Component.literal("Rotation: " + String.format("%.1f", yaw) + "°"), true);
        }
        return InteractionResult.SUCCESS;
      }

      MenuManager.getMenuHandler()
          .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPCEntity, 0);
      return InteractionResult.SUCCESS;
    }
    return InteractionResult.PASS;
  }

  @Override
  public InteractionResult useOn(UseOnContext userContext) {
    Level level = userContext.getLevel();
    Player player = userContext.getPlayer();
    if (player instanceof ServerPlayer serverPlayer) {
      BlockPos blockPos = userContext.getClickedPos();

      // Search for nearby EasyNPC entities above and below the block position
      AABB aabbAbove =
          new AABB(
              blockPos.getX() - 0.25d,
              blockPos.getY() - 2d,
              blockPos.getZ() - 0.25d,
              blockPos.getX() + 0.25d,
              blockPos.getY() + 2d,
              blockPos.getZ() + 0.25d);
      for (Mob mob : level.getEntitiesOfClass(Mob.class, aabbAbove.inflate(0.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // Search for nearby EasyNPC entities around the block position
      AABB aabbAround =
          new AABB(
              blockPos.getX() - 0.5d,
              blockPos.getY() - 0.5d,
              blockPos.getZ() - 0.5d,
              blockPos.getX() + 1d,
              blockPos.getY() + 1d,
              blockPos.getZ() + 1d);
      for (Mob mob :
          level.getEntitiesOfClass(Mob.class, aabbAround.inflate(0.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
          return InteractionResult.SUCCESS;
        }
      }

      // Expand the search area to find all nearby EasyNPC entities
      for (Mob mob :
          level.getEntitiesOfClass(Mob.class, aabbAround.inflate(2.5), Entity::isAlive)) {
        if (mob instanceof EasyNPCBase<?> easyNPC) {
          MenuManager.getMenuHandler()
              .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, easyNPC, 0);
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
    tooltipList.add(TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID));
    tooltipList.add(
        TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID + ".rotation_hint"));
  }
}
