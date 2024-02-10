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

package de.markusbordihn.easynpc.entity.easynpc;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.custom.CustomDataAccessor;
import de.markusbordihn.easynpc.entity.EasyNPCBaseEntity;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import net.minecraft.network.chat.Component;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.NeutralMob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import net.minecraft.world.entity.ai.navigation.GroundPathNavigation;
import net.minecraft.world.entity.npc.Npc;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface EasyNPC<T extends LivingEntity> extends Npc {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  static Class<? extends Entity> getSynchedEntityDataClass() {
    return EasyNPCBaseEntity.class;
  }

  Level getEasyNPCLevel();

  ServerLevel getEasyNPCServerLevel();

  EasyNPC<T> getEasyNPC();

  T getEasyNPCEntity();

  GoalSelector getEasyNPCGoalSelector();

  GoalSelector getEasyNPCTargetSelector();

  GroundPathNavigation getEasyNPCGroundPathNavigation();

  default NeutralMob getEasyNPCNeutralMob() {
    if (getEasyNPCEntity() instanceof NeutralMob neutralMob) {
      return neutralMob;
    }
    return null;
  }

  default AttackData<T> getEasyNPCAttackData() {
    return (AttackData<T>) this;
  }

  default SkinData<T> getEasyNPCSkinData() {
    return (SkinData<T>) this;
  }

  default ModelData<T> getEasyNPCModelData() {
    return (ModelData<T>) this;
  }

  default Component getEasyNPCTypeName() {
    return getEasyNPCEntity().getType().getDescription();
  }

  default void handlePlayerJoin(ServerPlayer serverPlayer) {}

  default void handlePlayerLeave(ServerPlayer serverPlayer) {}

  default void handleLivingEntityJoin(LivingEntity livingEntity) {}

  default void handleLivingEntityLeave(LivingEntity livingEntity) {}

  default void defineCustomData() {}

  <V> void setEasyNPCData(EntityDataAccessor<V> entityDataAccessor, V entityData);

  <V> V getEasyNPCData(EntityDataAccessor<V> entityDataAccessor);

  <V> void defineEasyNPCData(EntityDataAccessor<V> entityDataAccessor, V entityData);

  <V> void setEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor, V entityData);

  <V> V getEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor);

  <V> void defineEasyNPCCustomData(CustomDataAccessor<V> entityDataAccessor, V entityData);
}
