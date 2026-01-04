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

import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttackDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ProfessionDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ServerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TickerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.ActionHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.AttributeHandler;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BaseTickHandler;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.Saddleable;
import net.minecraft.world.entity.SpawnGroupData;

public interface EasyNPCBase<E extends PathfinderMob>
    extends Saddleable,
        EasyNPC<E>,
        ActionEventDataCapable<E>,
        ActionHandler<E>,
        AttackDataCapable<E>,
        AttributeDataCapable<E>,
        AttributeHandler<E>,
        BaseTickHandler<E>,
        ConfigDataCapable<E>,
        ConfigurationDataCapable<E>,
        DialogDataCapable<E>,
        DisplayAttributeDataCapable<E>,
        ModelDataCapable<E>,
        NavigationDataCapable<E>,
        ObjectiveDataCapable<E>,
        OwnerDataCapable<E>,
        PresetDataCapable<E>,
        ProfessionDataCapable<E>,
        RenderDataCapable<E>,
        ServerDataCapable<E>,
        SkinDataCapable<E>,
        SoundDataCapable<E>,
        StatusDataCapable<E>,
        TickerDataCapable<E>,
        TradingDataCapable<E>,
        VariantDataCapable<E> {

  default void registerEasyNPCDefaultVariant(Enum<?> variant) {
    log.info("Register default variant for {} with variant {} ...", this, variant);
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.setSkinVariantType(variant);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.registerDefaultSoundData(variant);
    }
  }

  default SpawnGroupData finalizeEasyNPCSpawn(SpawnGroupData spawnGroupData) {
    log.info("Finalize spawn for {} ...", this);

    // Set default navigation data.
    NavigationDataCapable<?> navigationData = getEasyNPCNavigationData();
    if (navigationData != null && !navigationData.hasHomePosition()) {
      navigationData.setHomePosition(this.getEntity().blockPosition());
    }

    // Skip next steps if NPC was already finalized.
    StatusDataCapable<?> statusData = getEasyNPCStatusData();
    if (statusData == null || !statusData.getStatusDataFlag(StatusDataType.FINALIZED)) {
      log.debug("Register default data for {} ...", this);

      // Register standard Objectives
      ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
      if (objectiveData != null) {
        objectiveData.registerStandardObjectives();
      }

      // Add default action interaction events
      ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
      if (actionEventData != null) {
        actionEventData.registerDefaultActionInteractionEvents();
      }
    } else {
      log.debug("Skip default data registration for {} ...", this);
    }

    return spawnGroupData;
  }

  default void defineEasyNPCBaseSyncedData(SynchedEntityData.Builder builder) {
    log.debug("Define synced data for {} with {}", this, builder);

    // First define variant data to ensure that all other data can be linked to the variant.
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.defineSynchedVariantData(builder);
    }

    // Define all other synced data.
    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineSynchedActionData(builder);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.defineSynchedAttackData(builder);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.defineSynchedAttributeData(builder);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineSynchedDialogData(builder);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.defineSynchedDisplayAttributeData(builder);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.defineSynchedModelData(builder);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.defineSynchedNavigationData(builder);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.defineSynchedOwnerData(builder);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.defineSynchedProfessionData(builder);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.defineSynchedRenderData(builder);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.defineSynchedSkinData(builder);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.defineSynchedSoundData(builder);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.defineSynchedTradingData(builder);
    }
  }

  default void defineEasyNPCBaseServerSideData() {
    if (!this.isServerSideInstance()) {
      return;
    }
    ServerDataCapable<E> serverData = getEasyNPCServerData();
    if (serverData == null) {
      log.error("No server data available for {}", this.getEntityUUID());
      return;
    }
    if (!serverData.hasServerEntityData()) {
      log.info("Register server-side data for {} ...", this.getEntityUUID());
      serverData.defineServerEntityData();
    }

    log.info("Define custom server-side data for {} ...", this.getEntityUUID());
    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.defineCustomActionData();
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.defineCustomDialogData();
    }
    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.defineCustomObjectiveData();
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.defineCustomPresetData();
    }
  }

  default void addEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    log.debug("Add additional save data for {} with {}", this, provider);
    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.addAdditionalActionData(compoundTag);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.addAdditionalAttackData(compoundTag);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.addAdditionalAttributeData(compoundTag);
    }
    ConfigDataCapable<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.addAdditionalConfigData(compoundTag);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.addAdditionalDialogData(compoundTag);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.addAdditionalDisplayAttributeData(compoundTag);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.addAdditionalModelData(compoundTag);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.addAdditionalNavigationData(compoundTag);
    }
    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.addAdditionalObjectiveData(compoundTag);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.addAdditionalOwnerData(compoundTag);
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.addAdditionalPresetData(compoundTag);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.addAdditionalProfessionData(compoundTag);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.addAdditionalRenderData(compoundTag);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.addAdditionalSkinData(compoundTag);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.addAdditionalSoundData(compoundTag);
    }
    StatusDataCapable<E> statusData = getEasyNPCStatusData();
    if (statusData != null) {
      statusData.addAdditionalStatusData(compoundTag);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.addAdditionalTradingData(compoundTag, provider);
    }
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.addAdditionalVariantData(compoundTag);
    }
  }

  default void readEasyNPCBaseAdditionalSaveData(
      CompoundTag compoundTag, HolderLookup.Provider provider) {
    log.debug("Read additional save data for {} with {}", this, provider);

    // First read important data to ensure that all other data can be linked to the variant.
    ConfigDataCapable<E> configData = getEasyNPCConfigData();
    if (configData != null) {
      configData.readAdditionalConfigData(compoundTag);
    }
    VariantDataCapable<E> variantData = getEasyNPCVariantData();
    if (variantData != null) {
      variantData.readAdditionalVariantData(compoundTag);
    }

    // Read all other synced data.
    ActionEventDataCapable<E> actionEventData = getEasyNPCActionEventData();
    if (actionEventData != null) {
      actionEventData.readAdditionalActionData(compoundTag);
    }
    AttackDataCapable<E> attackData = getEasyNPCAttackData();
    if (attackData != null) {
      attackData.readAdditionalAttackData(compoundTag);
    }
    AttributeDataCapable<E> attributeData = getEasyNPCAttributeData();
    if (attributeData != null) {
      attributeData.readAdditionalAttributeData(compoundTag);
    }
    DialogDataCapable<E> dialogData = getEasyNPCDialogData();
    if (dialogData != null) {
      dialogData.readAdditionalDialogData(compoundTag);
    }
    DisplayAttributeDataCapable<E> displayAttributeData = getEasyNPCDisplayAttributeData();
    if (displayAttributeData != null) {
      displayAttributeData.readAdditionalDisplayAttributeData(compoundTag);
    }
    ModelDataCapable<E> modelData = getEasyNPCModelData();
    if (modelData != null) {
      modelData.readAdditionalModelData(compoundTag);
    }
    NavigationDataCapable<E> navigationData = getEasyNPCNavigationData();
    if (navigationData != null) {
      navigationData.readAdditionalNavigationData(compoundTag);
    }
    OwnerDataCapable<E> ownerData = getEasyNPCOwnerData();
    if (ownerData != null) {
      ownerData.readAdditionalOwnerData(compoundTag);
    }
    PresetDataCapable<E> presetData = getEasyNPCPresetData();
    if (presetData != null) {
      presetData.readAdditionalPresetData(compoundTag);
    }
    ProfessionDataCapable<E> professionData = getEasyNPCProfessionData();
    if (professionData != null) {
      professionData.readAdditionalProfessionData(compoundTag);
    }
    RenderDataCapable<E> renderData = getEasyNPCRenderData();
    if (renderData != null) {
      renderData.readAdditionalRenderData(compoundTag);
    }
    SkinDataCapable<E> skinData = getEasyNPCSkinData();
    if (skinData != null) {
      skinData.readAdditionalSkinData(compoundTag);
    }
    SoundDataCapable<E> soundData = getEasyNPCSoundData();
    if (soundData != null) {
      soundData.readAdditionalSoundData(compoundTag);
    }
    StatusDataCapable<E> statusData = getEasyNPCStatusData();
    if (statusData != null) {
      statusData.readAdditionalStatusData(compoundTag);
    }
    TradingDataCapable<E> tradingData = getEasyNPCTradingData();
    if (tradingData != null) {
      tradingData.readAdditionalTradingData(compoundTag, provider);
    }

    // Register Objectives after all data is loaded.
    ObjectiveDataCapable<E> objectiveData = getEasyNPCObjectiveData();
    if (objectiveData != null) {
      objectiveData.readAdditionalObjectiveData(compoundTag);
    }

    // Refresh navigation data after all data is loaded.
    if (navigationData != null) {
      navigationData.refreshGroundNavigation();
    }
  }
}
