/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of easyNPC software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and easyNPC permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.data.trading.SafeMerchantData;
import java.util.UUID;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.profiling.ProfilerFiller;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.control.LookControl;
import net.minecraft.world.entity.monster.CrossbowAttackMob;
import net.minecraft.world.entity.monster.RangedAttackMob;
import net.minecraft.world.item.trading.Merchant;
import net.minecraft.world.level.Level;

public final class EasyNPCEntityAccess {

  public static <E extends PathfinderMob> LookControl getLookControl(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Mob mob ? mob.getLookControl() : null;
  }

  public static <E extends PathfinderMob> PathfinderMob getPathfinderMob(EasyNPC<E> easyNPC) {
    return easyNPC instanceof PathfinderMob pathfinderMob ? pathfinderMob : null;
  }

  public static <E extends PathfinderMob> Level getLevel(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Mob mob ? mob.level() : null;
  }

  public static <E extends PathfinderMob> ServerLevel getServerLevel(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Mob mob && mob.level() instanceof ServerLevel serverLevel
        ? serverLevel
        : null;
  }

  public static <E extends PathfinderMob> boolean isClientSide(EasyNPC<E> easyNPC) {
    return easyNPC.getEntityLevel() != null && easyNPC.getEntityLevel().isClientSide();
  }

  public static <E extends PathfinderMob> boolean isServerSide(EasyNPC<E> easyNPC) {
    return !isClientSide(easyNPC);
  }

  public static <E extends PathfinderMob> LivingEntity getLivingEntity(EasyNPC<E> easyNPC) {
    return easyNPC instanceof LivingEntity livingEntity ? livingEntity : null;
  }

  public static <E extends PathfinderMob> Merchant getMerchant(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Merchant
        ? new SafeMerchantData<>(easyNPC.getEasyNPCTradingData())
        : null;
  }

  public static <E extends PathfinderMob> CrossbowAttackMob getCrossbowAttackMob(
      EasyNPC<E> easyNPC) {
    return easyNPC instanceof CrossbowAttackMob crossbowAttackMob ? crossbowAttackMob : null;
  }

  public static <E extends PathfinderMob> RangedAttackMob getRangedAttackMob(EasyNPC<E> easyNPC) {
    return easyNPC instanceof RangedAttackMob rangedAttackMob ? rangedAttackMob : null;
  }

  public static <E extends PathfinderMob> ProfilerFiller getProfiler(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Mob mob ? mob.level().getProfiler() : null;
  }

  public static <E extends PathfinderMob> Entity getEntity(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Entity entity ? entity : null;
  }

  public static <E extends PathfinderMob> UUID getEntityUUID(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Entity entity ? entity.getUUID() : null;
  }

  public static <E extends PathfinderMob> Component getEntityTypeName(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Entity entity ? entity.getType().getDescription() : null;
  }

  public static <E extends PathfinderMob> EntityType<?> getEntityType(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Entity entity ? entity.getType() : null;
  }

  public static <E extends PathfinderMob> String getEntityTypeId(EasyNPC<E> easyNPC) {
    EntityType<?> entityType = getEntityType(easyNPC);
    if (entityType == null || !entityType.canSerialize()) {
      return null;
    }
    return EntityType.getKey(entityType).toString();
  }

  public static <E extends PathfinderMob> Mob getMob(EasyNPC<E> easyNPC) {
    return easyNPC instanceof Mob mob ? mob : null;
  }
}
