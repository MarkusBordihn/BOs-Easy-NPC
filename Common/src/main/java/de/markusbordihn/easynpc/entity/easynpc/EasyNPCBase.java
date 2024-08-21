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

import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackData;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigData;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationData;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogData;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeData;
import de.markusbordihn.easynpc.entity.easynpc.data.GuiData;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationData;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveData;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetData;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionData;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderData;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerData;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinData;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import de.markusbordihn.easynpc.entity.easynpc.data.SpawnerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerData;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantData;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import java.util.EnumMap;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.EntityDataAccessor;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Saddleable;
import net.minecraft.world.entity.SpawnGroupData;

public interface EasyNPCBase<E extends PathfinderMob>
    extends Saddleable,
        EasyNPC<E>,
        ActionHandler<E>,
        ActionEventData<E>,
        AttackData<E>,
        AttributeData<E>,
        BaseTickHandler<E>,
        ConfigurationData<E>,
        DialogData<E>,
        DisplayAttributeData<E>,
        GuiData<E>,
        NavigationData<E>,
        ConfigData<E>,
        ObjectiveData<E>,
        OwnerData<E>,
        PresetData<E>,
        ProfessionData<E>,
        RenderData<E>,
        ServerData<E>,
        SkinData<E>,
        SoundData<E>,
        SpawnerData<E>,
        TickerData<E>,
        TradingData<E>,
        VariantData<E> {

  static void registerEasyNPCSyncedData(
      EnumMap<SynchedDataIndex, EntityDataAccessor<?>> map, Class<? extends Entity> entityClass) {
    AttackData.registerSyncedAttackData(map, entityClass);
    AttributeData.registerSyncedAttributeData(map, entityClass);
    DisplayAttributeData.registerSyncedDisplayAttributeData(map, entityClass);
    NavigationData.registerSyncedNavigationData(map, entityClass);
    OwnerData.registerSyncedOwnerData(map, entityClass);
    ProfessionData.registerSyncedProfessionData(map, entityClass);
    RenderData.registerSyncedRenderData(map, entityClass);
    SkinData.registerSyncedSkinData(map, entityClass);
    SoundData.registerSyncedSoundData(map, entityClass);
    TradingData.registerSyncedTradingData(map, entityClass);
    VariantData.registerSyncedVariantData(map, entityClass);
  }

  default void registerEasyNPCDefaultHandler(Enum<?> variant) {
    log.info("Register default handler for {} with variant {} ...", this, variant);
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.setVariant(variant);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.registerDefaultAttributeData(variant);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default SpawnGroupData finalizeEasyNPCSpawn(SpawnGroupData spawnGroupData) {
    log.info("Finalize spawn for {} ...", this);

    // Set default navigation data.
    NavigationData<?> navigationData = getEasyNPCNavigationData();
    if (navigationData != null && !navigationData.hasHomePosition()) {
      navigationData.setHomePosition(this.getEntity().blockPosition());
    }

    // Register standard Objectives
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.registerStandardObjectives();
    }

    // Add default action interaction events
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.registerDefaultActionInteractionEvents();
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData(SynchedEntityData.Builder builder) {
    log.debug("Define synced data for {} with {}", this, builder);

    // First define variant data to ensure that all other data can be linked to the variant.
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData(builder);
    }

    // Define all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData(builder);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData(builder);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData(builder);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData(builder);
    }
    DisplayAttributeData<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData(builder);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData(builder);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData(builder);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData(builder);
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.defineSynchedRenderData(builder);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData(builder);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData(builder);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData(builder);
    }
  }

  default void defineEasyNPCBaseServerSideData() {
    if (!this.isServerSide()) {
      return;
    }
    ServerData<E> serverData = getEasyNPCServerData();
    if (serverData == null) {
      log.error("No server data available for {}", this);
      return;
    }
    if (!serverData.hasServerEntityData()) {
      log.info("Register server-side data for {} ...", this);
      serverData.defineServerEntityData();
    }

    log.info("Define custom server-side data for {} ...", this);
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineCustomActionData();
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineCustomDialogData();
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.defineCustomObjectiveData();
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.defineCustomSpawnerData();
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    log.debug("Add additional save data for {}", this);
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.addAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.addAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.addAdditionalAttributeData(compoundTag);
    }
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.addAdditionalConfigData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.addAdditionalDialogData(compoundTag);
    }
    DisplayAttributeData<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.addAdditionalDisplayAttributeData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.addAdditionalNavigationData(compoundTag);
    }
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.addAdditionalObjectiveData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.addAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.addAdditionalProfessionData(compoundTag);
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.addAdditionalRenderData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.addAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.addAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.addAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.addAdditionalTradingData(compoundTag, provider);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.addAdditionalVariantData(compoundTag);
    }
  }

  default void readEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    log.debug("Read additional save data for {} ...", this);

    // First read important data to ensure that all other data can be linked to the variant.
    ConfigData<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.readAdditionalConfigData(compoundTag);
    }
    VariantData<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.readAdditionalVariantData(compoundTag);
    }

    // Read all other synced data.
    ActionEventData<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.readAdditionalActionData(compoundTag);
    }
    AttackData<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.readAdditionalAttackData(compoundTag);
    }
    AttributeData<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.readAdditionalAttributeData(compoundTag);
    }
    DialogData<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.readAdditionalDialogData(compoundTag);
    }
    DisplayAttributeData<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.readAdditionalDisplayAttributeData(compoundTag);
    }
    NavigationData<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.readAdditionalNavigationData(compoundTag);
    }
    OwnerData<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.readAdditionalOwnerData(compoundTag);
    }
    ProfessionData<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.readAdditionalProfessionData(compoundTag);
    }
    RenderData<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.readAdditionalRenderData(compoundTag);
    }
    SkinData<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.readAdditionalSkinData(compoundTag);
    }
    SoundData<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.readAdditionalSoundData(compoundTag);
    }
    SpawnerData<E> spawnerData = getEasyNPCSpawnerData();
    if (spawnerData != null) {
      spawnerData.readAdditionalSpawnerData(compoundTag);
    }
    TradingData<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.readAdditionalTradingData(compoundTag, provider);
    }

    // Register Objectives after all data is loaded.
    ObjectiveData<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }

    // Refresh navigation data after all data is loaded.
    if (navigationData != null) {
      navigationData.refreshGroundNavigation();
    }
  }
}
